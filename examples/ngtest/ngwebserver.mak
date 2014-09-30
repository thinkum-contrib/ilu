# Microsoft Developer Studio Generated NMAKE File, Based on ngwebserver.dsp
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
!MESSAGE NMAKE /f "ngwebserver.mak" CFG="Win32 Debug"
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

CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

OUTDIR=.\WinRel
INTDIR=.\WinRel
# Begin Custom Macros
OutDir=.\WinRel
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\ngwebserver.exe"

!ELSE 

ALL : "$(OUTDIR)\ngwebserver.exe"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\iana_charsets_registry-common.obj"
    -@erase "$(INTDIR)\iana_charsets_registry-surrogate.obj"
    -@erase "$(INTDIR)\iana_charsets_registry-true.obj"
    -@erase "$(INTDIR)\iluhttp-common.obj"
    -@erase "$(INTDIR)\iluhttp-true.obj"
    -@erase "$(INTDIR)\iluhttp_webserv_impl.obj"
    -@erase "$(INTDIR)\ngbasic-common.obj"
    -@erase "$(INTDIR)\ngbasic-surrogate.obj"
    -@erase "$(INTDIR)\ngbasic-true.obj"
    -@erase "$(INTDIR)\ngbasic_webserv_impl.obj"
    -@erase "$(INTDIR)\ngcache-common.obj"
    -@erase "$(INTDIR)\ngcache-surrogate.obj"
    -@erase "$(INTDIR)\ngcache-true.obj"
    -@erase "$(INTDIR)\ngdocument-common.obj"
    -@erase "$(INTDIR)\ngdocument-true.obj"
    -@erase "$(INTDIR)\ngdocument_webserv_impl.obj"
    -@erase "$(INTDIR)\ngformprocessor-common.obj"
    -@erase "$(INTDIR)\ngformprocessor-true.obj"
    -@erase "$(INTDIR)\ngformprocessor_webserv_impl.obj"
    -@erase "$(INTDIR)\nglib.obj"
    -@erase "$(INTDIR)\ngproperty-common.obj"
    -@erase "$(INTDIR)\ngproperty-true.obj"
    -@erase "$(INTDIR)\ngproperty_webserv_impl.obj"
    -@erase "$(INTDIR)\ngrendering-common.obj"
    -@erase "$(INTDIR)\ngrendering-surrogate.obj"
    -@erase "$(INTDIR)\ngrendering-true.obj"
    -@erase "$(INTDIR)\ngrendering_webserv_impl.obj"
    -@erase "$(INTDIR)\ngstream-common.obj"
    -@erase "$(INTDIR)\ngstream-surrogate.obj"
    -@erase "$(INTDIR)\ngstream-true.obj"
    -@erase "$(INTDIR)\ngstream_webserv_impl.obj"
    -@erase "$(INTDIR)\ngwebserver.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(OUTDIR)\ngwebserver.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(ILUHOME)\include" /D "WIN32" /D "NDEBUG"\
 /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\ngwebserver.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\WinRel/
CPP_SBRS=.
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ngwebserver.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:no /pdb:"$(OUTDIR)\ngwebserver.pdb"\
 /machine:I386 /out:"$(OUTDIR)\ngwebserver.exe" 
LINK32_OBJS= \
    "$(INTDIR)\iana_charsets_registry-common.obj" \
    "$(INTDIR)\iana_charsets_registry-surrogate.obj" \
    "$(INTDIR)\iana_charsets_registry-true.obj" \
    "$(INTDIR)\iluhttp-common.obj" \
    "$(INTDIR)\iluhttp-true.obj" \
    "$(INTDIR)\iluhttp_webserv_impl.obj" \
    "$(INTDIR)\ngbasic-common.obj" \
    "$(INTDIR)\ngbasic-surrogate.obj" \
    "$(INTDIR)\ngbasic-true.obj" \
    "$(INTDIR)\ngbasic_webserv_impl.obj" \
    "$(INTDIR)\ngcache-common.obj" \
    "$(INTDIR)\ngcache-surrogate.obj" \
    "$(INTDIR)\ngcache-true.obj" \
    "$(INTDIR)\ngdocument-common.obj" \
    "$(INTDIR)\ngdocument-true.obj" \
    "$(INTDIR)\ngdocument_webserv_impl.obj" \
    "$(INTDIR)\ngformprocessor-common.obj" \
    "$(INTDIR)\ngformprocessor-true.obj" \
    "$(INTDIR)\ngformprocessor_webserv_impl.obj" \
    "$(INTDIR)\nglib.obj" \
    "$(INTDIR)\ngproperty-common.obj" \
    "$(INTDIR)\ngproperty-true.obj" \
    "$(INTDIR)\ngproperty_webserv_impl.obj" \
    "$(INTDIR)\ngrendering-common.obj" \
    "$(INTDIR)\ngrendering-surrogate.obj" \
    "$(INTDIR)\ngrendering-true.obj" \
    "$(INTDIR)\ngrendering_webserv_impl.obj" \
    "$(INTDIR)\ngstream-common.obj" \
    "$(INTDIR)\ngstream-surrogate.obj" \
    "$(INTDIR)\ngstream-true.obj" \
    "$(INTDIR)\ngstream_webserv_impl.obj" \
    "$(INTDIR)\ngwebserver.obj"

"$(OUTDIR)\ngwebserver.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

ALL : "$(OUTDIR)\ngwebserver.exe"

!ELSE 

ALL : "$(OUTDIR)\ngwebserver.exe"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\iana_charsets_registry-common.obj"
    -@erase "$(INTDIR)\iana_charsets_registry-surrogate.obj"
    -@erase "$(INTDIR)\iana_charsets_registry-true.obj"
    -@erase "$(INTDIR)\iluhttp-common.obj"
    -@erase "$(INTDIR)\iluhttp-true.obj"
    -@erase "$(INTDIR)\iluhttp_webserv_impl.obj"
    -@erase "$(INTDIR)\ngbasic-common.obj"
    -@erase "$(INTDIR)\ngbasic-surrogate.obj"
    -@erase "$(INTDIR)\ngbasic-true.obj"
    -@erase "$(INTDIR)\ngbasic_webserv_impl.obj"
    -@erase "$(INTDIR)\ngcache-common.obj"
    -@erase "$(INTDIR)\ngcache-surrogate.obj"
    -@erase "$(INTDIR)\ngcache-true.obj"
    -@erase "$(INTDIR)\ngdocument-common.obj"
    -@erase "$(INTDIR)\ngdocument-true.obj"
    -@erase "$(INTDIR)\ngdocument_webserv_impl.obj"
    -@erase "$(INTDIR)\ngformprocessor-common.obj"
    -@erase "$(INTDIR)\ngformprocessor-true.obj"
    -@erase "$(INTDIR)\ngformprocessor_webserv_impl.obj"
    -@erase "$(INTDIR)\nglib.obj"
    -@erase "$(INTDIR)\ngproperty-common.obj"
    -@erase "$(INTDIR)\ngproperty-true.obj"
    -@erase "$(INTDIR)\ngproperty_webserv_impl.obj"
    -@erase "$(INTDIR)\ngrendering-common.obj"
    -@erase "$(INTDIR)\ngrendering-surrogate.obj"
    -@erase "$(INTDIR)\ngrendering-true.obj"
    -@erase "$(INTDIR)\ngrendering_webserv_impl.obj"
    -@erase "$(INTDIR)\ngstream-common.obj"
    -@erase "$(INTDIR)\ngstream-surrogate.obj"
    -@erase "$(INTDIR)\ngstream-true.obj"
    -@erase "$(INTDIR)\ngstream_webserv_impl.obj"
    -@erase "$(INTDIR)\ngwebserver.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(INTDIR)\vc50.pdb"
    -@erase "$(OUTDIR)\ngwebserver.exe"
    -@erase "$(OUTDIR)\ngwebserver.ilk"
    -@erase "$(OUTDIR)\ngwebserver.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUHOME)\include" /D "WIN32" /D\
 "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\ngwebserver.pch" /YX\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ngwebserver.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=$(ILUHOME)\lib\iluc32.lib $(ILUHOME)\lib\ilu32.lib wsock32.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo\
 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\ngwebserver.pdb" /debug\
 /machine:I386 /out:"$(OUTDIR)\ngwebserver.exe" /pdbtype:sept 
LINK32_OBJS= \
    "$(INTDIR)\iana_charsets_registry-common.obj" \
    "$(INTDIR)\iana_charsets_registry-surrogate.obj" \
    "$(INTDIR)\iana_charsets_registry-true.obj" \
    "$(INTDIR)\iluhttp-common.obj" \
    "$(INTDIR)\iluhttp-true.obj" \
    "$(INTDIR)\iluhttp_webserv_impl.obj" \
    "$(INTDIR)\ngbasic-common.obj" \
    "$(INTDIR)\ngbasic-surrogate.obj" \
    "$(INTDIR)\ngbasic-true.obj" \
    "$(INTDIR)\ngbasic_webserv_impl.obj" \
    "$(INTDIR)\ngcache-common.obj" \
    "$(INTDIR)\ngcache-surrogate.obj" \
    "$(INTDIR)\ngcache-true.obj" \
    "$(INTDIR)\ngdocument-common.obj" \
    "$(INTDIR)\ngdocument-true.obj" \
    "$(INTDIR)\ngdocument_webserv_impl.obj" \
    "$(INTDIR)\ngformprocessor-common.obj" \
    "$(INTDIR)\ngformprocessor-true.obj" \
    "$(INTDIR)\ngformprocessor_webserv_impl.obj" \
    "$(INTDIR)\nglib.obj" \
    "$(INTDIR)\ngproperty-common.obj" \
    "$(INTDIR)\ngproperty-true.obj" \
    "$(INTDIR)\ngproperty_webserv_impl.obj" \
    "$(INTDIR)\ngrendering-common.obj" \
    "$(INTDIR)\ngrendering-surrogate.obj" \
    "$(INTDIR)\ngrendering-true.obj" \
    "$(INTDIR)\ngrendering_webserv_impl.obj" \
    "$(INTDIR)\ngstream-common.obj" \
    "$(INTDIR)\ngstream-surrogate.obj" \
    "$(INTDIR)\ngstream-true.obj" \
    "$(INTDIR)\ngstream_webserv_impl.obj" \
    "$(INTDIR)\ngwebserver.obj"

"$(OUTDIR)\ngwebserver.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
SOURCE=".\iana_charsets_registry-common.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_IANA_=\
    ".\iana_charsets_registry.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\iana_charsets_registry-common.obj" : $(SOURCE) $(DEP_CPP_IANA_)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_IANA_=\
    ".\iana_charsets_registry.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\iana_charsets_registry-common.obj" : $(SOURCE) $(DEP_CPP_IANA_)\
 "$(INTDIR)"


!ENDIF 

SOURCE=".\iana_charsets_registry-surrogate.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_IANA_C=\
    ".\iana_charsets_registry.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\iana_charsets_registry-surrogate.obj" : $(SOURCE) $(DEP_CPP_IANA_C)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_IANA_C=\
    ".\iana_charsets_registry.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\iana_charsets_registry-surrogate.obj" : $(SOURCE) $(DEP_CPP_IANA_C)\
 "$(INTDIR)"


!ENDIF 

SOURCE=".\iana_charsets_registry-true.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_IANA_CH=\
    ".\iana_charsets_registry.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\iana_charsets_registry-true.obj" : $(SOURCE) $(DEP_CPP_IANA_CH)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_IANA_CH=\
    ".\iana_charsets_registry.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\iana_charsets_registry-true.obj" : $(SOURCE) $(DEP_CPP_IANA_CH)\
 "$(INTDIR)"


!ENDIF 

SOURCE=".\iluhttp-common.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUHT=\
    ".\iluhttp.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\iluhttp-common.obj" : $(SOURCE) $(DEP_CPP_ILUHT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUHT=\
    ".\iluhttp.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\iluhttp-common.obj" : $(SOURCE) $(DEP_CPP_ILUHT) "$(INTDIR)"


!ENDIF 

SOURCE=".\iluhttp-true.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUHTT=\
    ".\iluhttp.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\iluhttp-true.obj" : $(SOURCE) $(DEP_CPP_ILUHTT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUHTT=\
    ".\iluhttp.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\iluhttp-true.obj" : $(SOURCE) $(DEP_CPP_ILUHTT) "$(INTDIR)"


!ENDIF 

SOURCE=.\iluhttp_webserv_impl.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ILUHTTP=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    ".\ngwebserver.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\iluhttp_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_ILUHTTP) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ILUHTTP=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    ".\ngwebserver.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\iluhttp_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_ILUHTTP) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngbasic-common.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGBAS=\
    ".\ngbasic.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngbasic-common.obj" : $(SOURCE) $(DEP_CPP_NGBAS) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGBAS=\
    ".\ngbasic.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngbasic-common.obj" : $(SOURCE) $(DEP_CPP_NGBAS) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngbasic-surrogate.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGBASI=\
    ".\ngbasic.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngbasic-surrogate.obj" : $(SOURCE) $(DEP_CPP_NGBASI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGBASI=\
    ".\ngbasic.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngbasic-surrogate.obj" : $(SOURCE) $(DEP_CPP_NGBASI) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngbasic-true.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGBASIC=\
    ".\ngbasic.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngbasic-true.obj" : $(SOURCE) $(DEP_CPP_NGBASIC) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGBASIC=\
    ".\ngbasic.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngbasic-true.obj" : $(SOURCE) $(DEP_CPP_NGBASIC) "$(INTDIR)"


!ENDIF 

SOURCE=.\ngbasic_webserv_impl.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGBASIC_=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngbasic_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_NGBASIC_)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGBASIC_=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngbasic_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_NGBASIC_)\
 "$(INTDIR)"


!ENDIF 

SOURCE=".\ngcache-common.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGCAC=\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngcache-common.obj" : $(SOURCE) $(DEP_CPP_NGCAC) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGCAC=\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngcache-common.obj" : $(SOURCE) $(DEP_CPP_NGCAC) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngcache-surrogate.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGCACH=\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngcache-surrogate.obj" : $(SOURCE) $(DEP_CPP_NGCACH) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGCACH=\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngcache-surrogate.obj" : $(SOURCE) $(DEP_CPP_NGCACH) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngcache-true.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGCACHE=\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngcache-true.obj" : $(SOURCE) $(DEP_CPP_NGCACHE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGCACHE=\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngcache-true.obj" : $(SOURCE) $(DEP_CPP_NGCACHE) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngdocument-common.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGDOC=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngdocument-common.obj" : $(SOURCE) $(DEP_CPP_NGDOC) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGDOC=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngdocument-common.obj" : $(SOURCE) $(DEP_CPP_NGDOC) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngdocument-true.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGDOCU=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngdocument-true.obj" : $(SOURCE) $(DEP_CPP_NGDOCU) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGDOCU=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngdocument-true.obj" : $(SOURCE) $(DEP_CPP_NGDOCU) "$(INTDIR)"


!ENDIF 

SOURCE=.\ngdocument_webserv_impl.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGDOCUM=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    ".\ngstream_webserv_impl.h"\
    ".\ngwebserver.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngdocument_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_NGDOCUM)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGDOCUM=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    ".\ngstream_webserv_impl.h"\
    ".\ngwebserver.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngdocument_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_NGDOCUM)\
 "$(INTDIR)"


!ENDIF 

SOURCE=".\ngformprocessor-common.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGFOR=\
    ".\iana_charsets_registry.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngformprocessor.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngformprocessor-common.obj" : $(SOURCE) $(DEP_CPP_NGFOR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGFOR=\
    ".\iana_charsets_registry.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngformprocessor.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngformprocessor-common.obj" : $(SOURCE) $(DEP_CPP_NGFOR) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngformprocessor-true.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGFORM=\
    ".\iana_charsets_registry.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngformprocessor.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngformprocessor-true.obj" : $(SOURCE) $(DEP_CPP_NGFORM) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGFORM=\
    ".\iana_charsets_registry.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngformprocessor.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngformprocessor-true.obj" : $(SOURCE) $(DEP_CPP_NGFORM) "$(INTDIR)"


!ENDIF 

SOURCE=.\ngformprocessor_webserv_impl.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGFORMP=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    ".\ngwebserver.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngformprocessor_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_NGFORMP)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGFORMP=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    ".\ngwebserver.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngformprocessor_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_NGFORMP)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\nglib.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGLIB=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iludebug.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\iluntrnl.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    "$(ILUHOME)\include\iluprotocol.h"\
    "$(ILUHOME)\include\ilutransport.h"\
    

"$(INTDIR)\nglib.obj" : $(SOURCE) $(DEP_CPP_NGLIB) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGLIB=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iludebug.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\iluntrnl.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    "$(ILUHOME)\include\iluprotocol.h"\
    "$(ILUHOME)\include\ilutransport.h"\
    

"$(INTDIR)\nglib.obj" : $(SOURCE) $(DEP_CPP_NGLIB) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngproperty-common.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGPRO=\
    ".\ngbasic.h"\
    ".\ngproperty.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngproperty-common.obj" : $(SOURCE) $(DEP_CPP_NGPRO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGPRO=\
    ".\ngbasic.h"\
    ".\ngproperty.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngproperty-common.obj" : $(SOURCE) $(DEP_CPP_NGPRO) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngproperty-true.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGPROP=\
    ".\ngbasic.h"\
    ".\ngproperty.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngproperty-true.obj" : $(SOURCE) $(DEP_CPP_NGPROP) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGPROP=\
    ".\ngbasic.h"\
    ".\ngproperty.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngproperty-true.obj" : $(SOURCE) $(DEP_CPP_NGPROP) "$(INTDIR)"


!ENDIF 

SOURCE=.\ngproperty_webserv_impl.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGPROPE=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    ".\ngwebserver.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngproperty_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_NGPROPE)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGPROPE=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    ".\ngwebserver.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngproperty_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_NGPROPE)\
 "$(INTDIR)"


!ENDIF 

SOURCE=".\ngrendering-common.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGREN=\
    ".\iana_charsets_registry.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngrendering-common.obj" : $(SOURCE) $(DEP_CPP_NGREN) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGREN=\
    ".\iana_charsets_registry.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngrendering-common.obj" : $(SOURCE) $(DEP_CPP_NGREN) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngrendering-surrogate.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGREND=\
    ".\iana_charsets_registry.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngrendering-surrogate.obj" : $(SOURCE) $(DEP_CPP_NGREND) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGREND=\
    ".\iana_charsets_registry.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngrendering-surrogate.obj" : $(SOURCE) $(DEP_CPP_NGREND) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngrendering-true.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGRENDE=\
    ".\iana_charsets_registry.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngrendering-true.obj" : $(SOURCE) $(DEP_CPP_NGRENDE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGRENDE=\
    ".\iana_charsets_registry.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngrendering-true.obj" : $(SOURCE) $(DEP_CPP_NGRENDE) "$(INTDIR)"


!ENDIF 

SOURCE=.\ngrendering_webserv_impl.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGRENDER=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngrendering_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_NGRENDER)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGRENDER=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngrendering_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_NGRENDER)\
 "$(INTDIR)"


!ENDIF 

SOURCE=".\ngstream-common.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGSTR=\
    ".\ngbasic.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngstream-common.obj" : $(SOURCE) $(DEP_CPP_NGSTR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGSTR=\
    ".\ngbasic.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngstream-common.obj" : $(SOURCE) $(DEP_CPP_NGSTR) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngstream-surrogate.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGSTRE=\
    ".\ngbasic.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngstream-surrogate.obj" : $(SOURCE) $(DEP_CPP_NGSTRE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGSTRE=\
    ".\ngbasic.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngstream-surrogate.obj" : $(SOURCE) $(DEP_CPP_NGSTRE) "$(INTDIR)"


!ENDIF 

SOURCE=".\ngstream-true.c"

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGSTREA=\
    ".\ngbasic.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngstream-true.obj" : $(SOURCE) $(DEP_CPP_NGSTREA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGSTREA=\
    ".\ngbasic.h"\
    ".\ngstream.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngstream-true.obj" : $(SOURCE) $(DEP_CPP_NGSTREA) "$(INTDIR)"


!ENDIF 

SOURCE=.\ngstream_webserv_impl.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGSTREAM=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    ".\ngstream_webserv_impl.h"\
    ".\ngwebserver.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngstream_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_NGSTREAM)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGSTREAM=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    ".\ngstream_webserv_impl.h"\
    ".\ngwebserver.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngstream_webserv_impl.obj" : $(SOURCE) $(DEP_CPP_NGSTREAM)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\ngwebserver.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NGWEB=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    ".\ngwebserver.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngwebserver.obj" : $(SOURCE) $(DEP_CPP_NGWEB) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NGWEB=\
    ".\iana_charsets_registry.h"\
    ".\iluhttp.h"\
    ".\ngbasic.h"\
    ".\ngcache.h"\
    ".\ngdocument.h"\
    ".\ngformprocessor.h"\
    ".\nglib.h"\
    ".\ngproperty.h"\
    ".\ngrendering.h"\
    ".\ngstream.h"\
    ".\ngwebserver.h"\
    "$(ILUHOME)\include\gssapi.h"\
    "$(ILUHOME)\include\ilubasic.h"\
    "$(ILUHOME)\include\iluchdrs.h"\
    "$(ILUHOME)\include\ilucstub.h"\
    "$(ILUHOME)\include\iluerror.h"\
    "$(ILUHOME)\include\iluerrs.h"\
    "$(ILUHOME)\include\ilugsswin_conf.h"\
    "$(ILUHOME)\include\iluhash.h"\
    "$(ILUHOME)\include\ilutpcod.h"\
    "$(ILUHOME)\include\ilutypes.h"\
    "$(ILUHOME)\include\iluwin.h"\
    "$(ILUHOME)\include\iluxport.h"\
    

"$(INTDIR)\ngwebserver.obj" : $(SOURCE) $(DEP_CPP_NGWEB) "$(INTDIR)"


!ENDIF 


!ENDIF 

