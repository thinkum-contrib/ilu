# Microsoft Developer Studio Generated NMAKE File, Based on cpp2server.dsp
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
!MESSAGE NMAKE /f "cpp2server.mak" CFG="Win32 Debug"
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
# Begin Custom Macros
OutDir=.\WinRel
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\cpp2server.exe"

!ELSE 

ALL : "$(OUTDIR)\cpp2server.exe"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\cpp2srvr.obj"
    -@erase "$(INTDIR)\Test1-cpp.obj"
    -@erase "$(INTDIR)\Test1-cpptrue.obj"
    -@erase "$(INTDIR)\Test2-cpp.obj"
    -@erase "$(INTDIR)\Test2-cpptrue.obj"
    -@erase "$(INTDIR)\Test3-cpp.obj"
    -@erase "$(INTDIR)\Test3-cpptrue.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(OUTDIR)\cpp2server.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D\
 "_MBCS" /Fp"$(INTDIR)\cpp2server.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD\
 /c 
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\cpp2server.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\ilucpp2.lib $(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:no /pdb:"$(OUTDIR)\cpp2server.pdb"\
 /machine:I386 /out:"$(OUTDIR)\cpp2server.exe" 
LINK32_OBJS= \
    "$(INTDIR)\cpp2srvr.obj" \
    "$(INTDIR)\Test1-cpp.obj" \
    "$(INTDIR)\Test1-cpptrue.obj" \
    "$(INTDIR)\Test2-cpp.obj" \
    "$(INTDIR)\Test2-cpptrue.obj" \
    "$(INTDIR)\Test3-cpp.obj" \
    "$(INTDIR)\Test3-cpptrue.obj"

"$(OUTDIR)\cpp2server.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

ALL : "$(OUTDIR)\cpp2server.exe"

!ELSE 

ALL : "$(OUTDIR)\cpp2server.exe"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\cpp2srvr.obj"
    -@erase "$(INTDIR)\Test1-cpp.obj"
    -@erase "$(INTDIR)\Test1-cpptrue.obj"
    -@erase "$(INTDIR)\Test2-cpp.obj"
    -@erase "$(INTDIR)\Test2-cpptrue.obj"
    -@erase "$(INTDIR)\Test3-cpp.obj"
    -@erase "$(INTDIR)\Test3-cpptrue.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(INTDIR)\vc50.pdb"
    -@erase "$(OUTDIR)\cpp2server.exe"
    -@erase "$(OUTDIR)\cpp2server.ilk"
    -@erase "$(OUTDIR)\cpp2server.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE"\
 /D "_MBCS" /Fp"$(INTDIR)\cpp2server.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\cpp2server.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\ilucpp2.lib $(ILUHOME)\lib\ilu32.lib $(ILUHOME)\lib\gss.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\cpp2server.pdb" /debug\
 /machine:I386 /out:"$(OUTDIR)\cpp2server.exe" /pdbtype:sept 
LINK32_OBJS= \
    "$(INTDIR)\cpp2srvr.obj" \
    "$(INTDIR)\Test1-cpp.obj" \
    "$(INTDIR)\Test1-cpptrue.obj" \
    "$(INTDIR)\Test2-cpp.obj" \
    "$(INTDIR)\Test2-cpptrue.obj" \
    "$(INTDIR)\Test3-cpp.obj" \
    "$(INTDIR)\Test3-cpptrue.obj"

"$(OUTDIR)\cpp2server.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(CFG)" == "Win32 Release" || "$(CFG)" ==\
 "Win32 Debug"
SOURCE=.\cpp2srvr.cpp

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_CPP2S=\
    ".\test1-cpp.hpp"\
    ".\test1-cpptrue.hpp"\
    ".\test2-cpp.hpp"\
    ".\test2-cpptrue.hpp"\
    ".\test3-cpp.hpp"\
    ".\test3-cpptrue.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\cpp2srvr.obj" : $(SOURCE) $(DEP_CPP_CPP2S) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_CPP2S=\
    ".\test1-cpp.hpp"\
    ".\test1-cpptrue.hpp"\
    ".\test2-cpp.hpp"\
    ".\test2-cpptrue.hpp"\
    ".\test3-cpp.hpp"\
    ".\test3-cpptrue.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\cpp2srvr.obj" : $(SOURCE) $(DEP_CPP_CPP2S) "$(INTDIR)"


!ENDIF 

SOURCE=".\Test1-cpp.cpp"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_TEST1=\
    ".\test1-cpp.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\Test1-cpp.obj" : $(SOURCE) $(DEP_CPP_TEST1) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_TEST1=\
    ".\test1-cpp.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\Test1-cpp.obj" : $(SOURCE) $(DEP_CPP_TEST1) "$(INTDIR)"


!ENDIF 

SOURCE=".\Test1-cpptrue.cpp"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_TEST1_=\
    ".\test1-cpp.hpp"\
    ".\test1-cpptrue.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\Test1-cpptrue.obj" : $(SOURCE) $(DEP_CPP_TEST1_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_TEST1_=\
    ".\test1-cpp.hpp"\
    ".\test1-cpptrue.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\Test1-cpptrue.obj" : $(SOURCE) $(DEP_CPP_TEST1_) "$(INTDIR)"


!ENDIF 

SOURCE=".\Test2-cpp.cpp"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_TEST2=\
    ".\test1-cpp.hpp"\
    ".\test2-cpp.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\Test2-cpp.obj" : $(SOURCE) $(DEP_CPP_TEST2) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_TEST2=\
    ".\test1-cpp.hpp"\
    ".\test2-cpp.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\Test2-cpp.obj" : $(SOURCE) $(DEP_CPP_TEST2) "$(INTDIR)"


!ENDIF 

SOURCE=".\Test2-cpptrue.cpp"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_TEST2_=\
    ".\test1-cpp.hpp"\
    ".\test2-cpp.hpp"\
    ".\test2-cpptrue.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\Test2-cpptrue.obj" : $(SOURCE) $(DEP_CPP_TEST2_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_TEST2_=\
    ".\test1-cpp.hpp"\
    ".\test2-cpp.hpp"\
    ".\test2-cpptrue.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\Test2-cpptrue.obj" : $(SOURCE) $(DEP_CPP_TEST2_) "$(INTDIR)"


!ENDIF 

SOURCE=".\Test3-cpp.cpp"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_TEST3=\
    ".\test1-cpp.hpp"\
    ".\test2-cpp.hpp"\
    ".\test3-cpp.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\Test3-cpp.obj" : $(SOURCE) $(DEP_CPP_TEST3) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_TEST3=\
    ".\test1-cpp.hpp"\
    ".\test2-cpp.hpp"\
    ".\test3-cpp.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\Test3-cpp.obj" : $(SOURCE) $(DEP_CPP_TEST3) "$(INTDIR)"


!ENDIF 

SOURCE=".\Test3-cpptrue.cpp"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_TEST3_=\
    ".\test1-cpp.hpp"\
    ".\test2-cpp.hpp"\
    ".\test3-cpp.hpp"\
    ".\test3-cpptrue.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\Test3-cpptrue.obj" : $(SOURCE) $(DEP_CPP_TEST3_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_TEST3_=\
    ".\test1-cpp.hpp"\
    ".\test2-cpp.hpp"\
    ".\test3-cpp.hpp"\
    ".\test3-cpptrue.hpp"\
    {$(INCLUDE)}"corba-templates.hpp"\
    {$(INCLUDE)}"corba.hpp"\
    {$(INCLUDE)}"cppportability.hpp"\
    {$(INCLUDE)}"ilu.hpp"\
    {$(INCLUDE)}"ilubasic.h"\
    {$(INCLUDE)}"iludebug.h"\
    {$(INCLUDE)}"iluerror.h"\
    {$(INCLUDE)}"iluerrs.h"\
    {$(INCLUDE)}"iluhash.h"\
    {$(INCLUDE)}"ilutpcod.h"\
    {$(INCLUDE)}"ilutypes.h"\
    {$(INCLUDE)}"iluwin.h"\
    {$(INCLUDE)}"iluxport.h"\
    

"$(INTDIR)\Test3-cpptrue.obj" : $(SOURCE) $(DEP_CPP_TEST3_) "$(INTDIR)"


!ENDIF 


!ENDIF 

