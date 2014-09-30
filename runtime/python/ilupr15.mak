# Microsoft Developer Studio Generated NMAKE File, Based on ilupr15.dsp
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
!MESSAGE NMAKE /f "ilupr15.mak" CFG="Win32 Debug"
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

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

OUTDIR=$(ILUHOME)\lib
INTDIR=.\WinRel
# Begin Custom Macros
OutDir=$(ILUHOME)\lib
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\iluPr.pyd"

!ELSE 

ALL : "$(OUTDIR)\iluPr.pyd"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\ilualobject.obj"
    -@erase "$(INTDIR)\ilucaobject.obj"
    -@erase "$(INTDIR)\iluclobject.obj"
    -@erase "$(INTDIR)\iluftobject.obj"
    -@erase "$(INTDIR)\ilugiobject.obj"
    -@erase "$(INTDIR)\ilulpobject.obj"
    -@erase "$(INTDIR)\ilulrobject.obj"
    -@erase "$(INTDIR)\iluplobject.obj"
    -@erase "$(INTDIR)\iluppobject.obj"
    -@erase "$(INTDIR)\iluPrmodule.obj"
    -@erase "$(INTDIR)\ilupygss.obj"
    -@erase "$(INTDIR)\ilusvobject.obj"
    -@erase "$(INTDIR)\iluszobject.obj"
    -@erase "$(INTDIR)\ilutpobject.obj"
    -@erase "$(INTDIR)\iohcobject.obj"
    -@erase "$(INTDIR)\ivobject.obj"
    -@erase "$(INTDIR)\thcobject.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(OUTDIR)\ilupr.exp"
    -@erase "$(OUTDIR)\iluPr.pyd"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(PYTHONSRC)\pc" /I\
 "$(PYTHONSRC)\include" /I "$(ILUSRC)\runtime\kernel" /I\
 "$(ILUHOME)\GSS\kernel" /I "$(PYTHONSRC)\pc" /I "$(PYTHONSRC)\include" /I\
 "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /D "WIN32" /D "ILU_BUILDING_RUNTIME" /D "NDEBUG" /D\
 "_WINDOWS" /Fp"$(INTDIR)\ilupr15.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD\
 /c /Tp 
CPP_OBJS=.\WinRel/
CPP_SBRS=.
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ilupr15.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=$(PYTHONSRC)\PCbuild\python15.lib $(ILUHOME)\lib\ilu32.lib\
 $(ILUHOME)\lib\gss.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)\ilupr.pdb" /machine:I386 /out:"$(OUTDIR)\iluPr.pyd"\
 /implib:"$(OUTDIR)\ilupr.lib" /export:initiluPr \
 /export:ilupython_GetPerThreadDataTech /export:ilupython_ForkNewThread
LINK32_OBJS= \
    "$(INTDIR)\ilualobject.obj" \
    "$(INTDIR)\ilucaobject.obj" \
    "$(INTDIR)\iluclobject.obj" \
    "$(INTDIR)\iluftobject.obj" \
    "$(INTDIR)\ilugiobject.obj" \
    "$(INTDIR)\ilulpobject.obj" \
    "$(INTDIR)\ilulrobject.obj" \
    "$(INTDIR)\iluplobject.obj" \
    "$(INTDIR)\iluppobject.obj" \
    "$(INTDIR)\iluPrmodule.obj" \
    "$(INTDIR)\ilupygss.obj" \
    "$(INTDIR)\ilusvobject.obj" \
    "$(INTDIR)\iluszobject.obj" \
    "$(INTDIR)\ilutpobject.obj" \
    "$(INTDIR)\iohcobject.obj" \
    "$(INTDIR)\ivobject.obj" \
    "$(INTDIR)\thcobject.obj"

"$(OUTDIR)\iluPr.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

OUTDIR=$(ILUHOME)\lib
INTDIR=.\WinDebug
# Begin Custom Macros
OutDir=$(ILUHOME)\lib
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\iluPr.pyd"

!ELSE 

ALL : "$(OUTDIR)\iluPr.pyd"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\ilualobject.obj"
    -@erase "$(INTDIR)\ilucaobject.obj"
    -@erase "$(INTDIR)\iluclobject.obj"
    -@erase "$(INTDIR)\iluftobject.obj"
    -@erase "$(INTDIR)\ilugiobject.obj"
    -@erase "$(INTDIR)\ilulpobject.obj"
    -@erase "$(INTDIR)\ilulrobject.obj"
    -@erase "$(INTDIR)\iluplobject.obj"
    -@erase "$(INTDIR)\iluppobject.obj"
    -@erase "$(INTDIR)\iluPrmodule.obj"
    -@erase "$(INTDIR)\ilupygss.obj"
    -@erase "$(INTDIR)\ilusvobject.obj"
    -@erase "$(INTDIR)\iluszobject.obj"
    -@erase "$(INTDIR)\ilutpobject.obj"
    -@erase "$(INTDIR)\iohcobject.obj"
    -@erase "$(INTDIR)\ivobject.obj"
    -@erase "$(INTDIR)\thcobject.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(INTDIR)\vc50.pdb"
    -@erase "$(OUTDIR)\ilupr.exp"
    -@erase "$(OUTDIR)\ilupr.ilk"
    -@erase "$(OUTDIR)\ilupr.pdb"
    -@erase "$(OUTDIR)\iluPr.pyd"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "$(PYTHONSRC)\pc" /I\
 "$(PYTHONSRC)\include" /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel"\
 /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "ILU_BUILDING_RUNTIME" /Fp"$(INTDIR)\ilupr15.pch" /YX\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c /Tp 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ilupr15.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=$(PYTHONSRC)\PCbuild\python15.lib $(ILUHOME)\lib\ilu32.lib\
 $(ILUHOME)\lib\gss.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:yes\
 /pdb:"$(OUTDIR)\ilupr.pdb" /debug /machine:I386 /out:"$(OUTDIR)\iluPr.pyd"\
 /implib:"$(OUTDIR)\ilupr.lib" /pdbtype:sept /export:initiluPr \
 /export:ilupython_GetPerThreadDataTech /export:ilupython_ForkNewThread
LINK32_OBJS= \
    "$(INTDIR)\ilualobject.obj" \
    "$(INTDIR)\ilucaobject.obj" \
    "$(INTDIR)\iluclobject.obj" \
    "$(INTDIR)\iluftobject.obj" \
    "$(INTDIR)\ilugiobject.obj" \
    "$(INTDIR)\ilulpobject.obj" \
    "$(INTDIR)\ilulrobject.obj" \
    "$(INTDIR)\iluplobject.obj" \
    "$(INTDIR)\iluppobject.obj" \
    "$(INTDIR)\iluPrmodule.obj" \
    "$(INTDIR)\ilupygss.obj" \
    "$(INTDIR)\ilusvobject.obj" \
    "$(INTDIR)\iluszobject.obj" \
    "$(INTDIR)\ilutpobject.obj" \
    "$(INTDIR)\iohcobject.obj" \
    "$(INTDIR)\ivobject.obj" \
    "$(INTDIR)\thcobject.obj"

"$(OUTDIR)\iluPr.pyd" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
SOURCE=$(ILUSRC)\runtime\python\ilualobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUAL=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilualobject.h"\
    "$(ILUSRC)\runtime\python\iluftobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonthreads.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilualobject.obj" : $(SOURCE) $(DEP_CPP_ILUAL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUAL=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilualobject.h"\
    "$(ILUSRC)\runtime\python\iluftobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonthreads.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilualobject.obj" : $(SOURCE) $(DEP_CPP_ILUAL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\ilucaobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUCA=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilucaobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilucaobject.obj" : $(SOURCE) $(DEP_CPP_ILUCA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUCA=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilucaobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilucaobject.obj" : $(SOURCE) $(DEP_CPP_ILUCA) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\iluclobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUCL=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\iluclobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iluclobject.obj" : $(SOURCE) $(DEP_CPP_ILUCL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUCL=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\iluclobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iluclobject.obj" : $(SOURCE) $(DEP_CPP_ILUCL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\iluftobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUFT=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\iluftobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iluftobject.obj" : $(SOURCE) $(DEP_CPP_ILUFT) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUFT=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\iluftobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iluftobject.obj" : $(SOURCE) $(DEP_CPP_ILUFT) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\ilugiobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUGI=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilugiobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilugiobject.obj" : $(SOURCE) $(DEP_CPP_ILUGI) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUGI=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilugiobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilugiobject.obj" : $(SOURCE) $(DEP_CPP_ILUGI) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\ilulpobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILULP=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilulpobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilulpobject.obj" : $(SOURCE) $(DEP_CPP_ILULP) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILULP=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilulpobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilulpobject.obj" : $(SOURCE) $(DEP_CPP_ILULP) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\ilulrobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILULR=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilulrobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilulrobject.obj" : $(SOURCE) $(DEP_CPP_ILULR) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILULR=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilulrobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilulrobject.obj" : $(SOURCE) $(DEP_CPP_ILULR) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\iluplobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUPL=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\iluplobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iluplobject.obj" : $(SOURCE) $(DEP_CPP_ILUPL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUPL=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\iluplobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iluplobject.obj" : $(SOURCE) $(DEP_CPP_ILUPL) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\iluppobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUPP=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilugiobject.h"\
    "$(ILUSRC)\runtime\python\iluppobject.h"\
    "$(ILUSRC)\runtime\python\ilupygss.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iluppobject.obj" : $(SOURCE) $(DEP_CPP_ILUPP) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUPP=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilugiobject.h"\
    "$(ILUSRC)\runtime\python\iluppobject.h"\
    "$(ILUSRC)\runtime\python\ilupygss.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iluppobject.obj" : $(SOURCE) $(DEP_CPP_ILUPP) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\iluPrmodule.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUPR=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\kernel\os\win.h"\
    "$(ILUSRC)\runtime\kernel\oscalls.h"\
    "$(ILUSRC)\runtime\python\ilualobject.h"\
    "$(ILUSRC)\runtime\python\ilucaobject.h"\
    "$(ILUSRC)\runtime\python\iluclobject.h"\
    "$(ILUSRC)\runtime\python\iluftobject.h"\
    "$(ILUSRC)\runtime\python\ilugiobject.h"\
    "$(ILUSRC)\runtime\python\ilulpobject.h"\
    "$(ILUSRC)\runtime\python\ilulrobject.h"\
    "$(ILUSRC)\runtime\python\iluplobject.h"\
    "$(ILUSRC)\runtime\python\iluppobject.h"\
    "$(ILUSRC)\runtime\python\ilupygss.h"\
    "$(ILUSRC)\runtime\python\ilusvobject.h"\
    "$(ILUSRC)\runtime\python\iluszobject.h"\
    "$(ILUSRC)\runtime\python\ilutpobject.h"\
    "$(ILUSRC)\runtime\python\iohcobject.h"\
    "$(ILUSRC)\runtime\python\ivobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonthreads.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(ILUSRC)\runtime\python\thcobject.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\thread.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iluPrmodule.obj" : $(SOURCE) $(DEP_CPP_ILUPR) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUPR=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\kernel\os\win.h"\
    "$(ILUSRC)\runtime\kernel\oscalls.h"\
    "$(ILUSRC)\runtime\python\ilualobject.h"\
    "$(ILUSRC)\runtime\python\ilucaobject.h"\
    "$(ILUSRC)\runtime\python\iluclobject.h"\
    "$(ILUSRC)\runtime\python\iluftobject.h"\
    "$(ILUSRC)\runtime\python\ilugiobject.h"\
    "$(ILUSRC)\runtime\python\ilulpobject.h"\
    "$(ILUSRC)\runtime\python\ilulrobject.h"\
    "$(ILUSRC)\runtime\python\iluplobject.h"\
    "$(ILUSRC)\runtime\python\iluppobject.h"\
    "$(ILUSRC)\runtime\python\ilupygss.h"\
    "$(ILUSRC)\runtime\python\ilusvobject.h"\
    "$(ILUSRC)\runtime\python\iluszobject.h"\
    "$(ILUSRC)\runtime\python\ilutpobject.h"\
    "$(ILUSRC)\runtime\python\iohcobject.h"\
    "$(ILUSRC)\runtime\python\ivobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonthreads.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(ILUSRC)\runtime\python\thcobject.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\thread.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iluPrmodule.obj" : $(SOURCE) $(DEP_CPP_ILUPR) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\ilupygss.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUPY=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilupygss.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilupygss.obj" : $(SOURCE) $(DEP_CPP_ILUPY) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUPY=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilupygss.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilupygss.obj" : $(SOURCE) $(DEP_CPP_ILUPY) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\ilusvobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUSV=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilusvobject.h"\
    "$(ILUSRC)\runtime\python\iluszobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilusvobject.obj" : $(SOURCE) $(DEP_CPP_ILUSV) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUSV=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilusvobject.h"\
    "$(ILUSRC)\runtime\python\iluszobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilusvobject.obj" : $(SOURCE) $(DEP_CPP_ILUSV) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\iluszobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUSZ=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\iluszobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iluszobject.obj" : $(SOURCE) $(DEP_CPP_ILUSZ) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUSZ=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\iluszobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iluszobject.obj" : $(SOURCE) $(DEP_CPP_ILUSZ) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\ilutpobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUTP=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilutpobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilutpobject.obj" : $(SOURCE) $(DEP_CPP_ILUTP) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUTP=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilutpobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ilutpobject.obj" : $(SOURCE) $(DEP_CPP_ILUTP) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\iohcobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_IOHCO=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\iohcobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iohcobject.obj" : $(SOURCE) $(DEP_CPP_IOHCO) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_IOHCO=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\iohcobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\iohcobject.obj" : $(SOURCE) $(DEP_CPP_IOHCO) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\ivobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_IVOBJ=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilusvobject.h"\
    "$(ILUSRC)\runtime\python\ivobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonthreads.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ivobject.obj" : $(SOURCE) $(DEP_CPP_IVOBJ) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_IVOBJ=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\ilusvobject.h"\
    "$(ILUSRC)\runtime\python\ivobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonthreads.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\ivobject.obj" : $(SOURCE) $(DEP_CPP_IVOBJ) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\runtime\python\thcobject.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_THCOB=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\iluftobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(ILUSRC)\runtime\python\thcobject.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\thcobject.obj" : $(SOURCE) $(DEP_CPP_THCOB) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_THCOB=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugsswin_conf.h"\
    "$(ILUSRC)\runtime\kernel\ilubasic.h"\
    "$(ILUSRC)\runtime\kernel\iluerror.h"\
    "$(ILUSRC)\runtime\kernel\iluerrs.h"\
    "$(ILUSRC)\runtime\kernel\iluhash.h"\
    "$(ILUSRC)\runtime\kernel\ilutpcod.h"\
    "$(ILUSRC)\runtime\kernel\ilutypes.h"\
    "$(ILUSRC)\runtime\kernel\iluwin.h"\
    "$(ILUSRC)\runtime\kernel\iluxport.h"\
    "$(ILUSRC)\runtime\python\iluftobject.h"\
    "$(ILUSRC)\runtime\python\python.h"\
    "$(ILUSRC)\runtime\python\pythonversion.h"\
    "$(ILUSRC)\runtime\python\thcobject.h"\
    "$(PYTHONSRC)\include\abstract.h"\
    "$(PYTHONSRC)\include\ceval.h"\
    "$(PYTHONSRC)\include\classobject.h"\
    "$(PYTHONSRC)\include\cobject.h"\
    "$(PYTHONSRC)\include\complexobject.h"\
    "$(PYTHONSRC)\include\dictobject.h"\
    "$(PYTHONSRC)\include\fileobject.h"\
    "$(PYTHONSRC)\include\floatobject.h"\
    "$(PYTHONSRC)\include\funcobject.h"\
    "$(PYTHONSRC)\include\import.h"\
    "$(PYTHONSRC)\include\intobject.h"\
    "$(PYTHONSRC)\include\intrcheck.h"\
    "$(PYTHONSRC)\include\listobject.h"\
    "$(PYTHONSRC)\include\longobject.h"\
    "$(PYTHONSRC)\include\methodobject.h"\
    "$(PYTHONSRC)\include\modsupport.h"\
    "$(PYTHONSRC)\include\moduleobject.h"\
    "$(PYTHONSRC)\include\mymalloc.h"\
    "$(PYTHONSRC)\include\myproto.h"\
    "$(PYTHONSRC)\include\object.h"\
    "$(PYTHONSRC)\include\objimpl.h"\
    "$(PYTHONSRC)\include\pydebug.h"\
    "$(PYTHONSRC)\include\pyerrors.h"\
    "$(PYTHONSRC)\include\pyfpe.h"\
    "$(PYTHONSRC)\include\pystate.h"\
    "$(PYTHONSRC)\include\python.h"\
    "$(PYTHONSRC)\include\pythonrun.h"\
    "$(PYTHONSRC)\include\rangeobject.h"\
    "$(PYTHONSRC)\include\sliceobject.h"\
    "$(PYTHONSRC)\include\stringobject.h"\
    "$(PYTHONSRC)\include\sysmodule.h"\
    "$(PYTHONSRC)\include\traceback.h"\
    "$(PYTHONSRC)\include\tupleobject.h"\
    "$(PYTHONSRC)\pc\config.h"\
    

"$(INTDIR)\thcobject.obj" : $(SOURCE) $(DEP_CPP_THCOB) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

