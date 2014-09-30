# Microsoft Developer Studio Generated NMAKE File, Based on gss.dsp
!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified. Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "gss.mak" CFG="Win32 Debug"
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

!IF  "$(CFG)" == "Win32 Release"

OUTDIR=$(ILUHOME)\bin
INTDIR=$(ILUSRC)\GSS\kernel\WinRel
# Begin Custom Macros
OutDir=$(ILUHOME)\bin
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\gss.dll"

!ELSE 

ALL : "$(OUTDIR)\gss.dll"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\anon_ns.obj"
    -@erase "$(INTDIR)\gss_asn1.obj"
    -@erase "$(INTDIR)\gss_debug.obj"
    -@erase "$(INTDIR)\gss_ext.obj"
    -@erase "$(INTDIR)\gss_impl.obj"
    -@erase "$(INTDIR)\gss_oidtbl.obj"
    -@erase "$(INTDIR)\gss_util.obj"
    -@erase "$(INTDIR)\gss_x500_ns.obj"
    -@erase "$(INTDIR)\nil_scheme.obj"
    -@erase "$(INTDIR)\rfc822_ns.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(OUTDIR)\gss.dll"
    -@erase "$(OUTDIR)\gss.exp"
    -@erase "$(ILUHOME)\lib\gss.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "$(ILUSRC)\GSS\kernel" /D "WIN32" /D\
 "NDEBUG" /D "_WINDOWS" /D "GSS_BUILDING_KERNEL" /Fp"$(INTDIR)\gss.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=$(ILUSRC)\GSS\kernel\WinRel/
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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\gss.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)\gss.pdb" /machine:I386 /out:"$(OUTDIR)\gss.dll"\
 /implib:"$(ILUHOME)\lib\gss.lib" 
LINK32_OBJS= \
    "$(INTDIR)\anon_ns.obj" \
    "$(INTDIR)\gss_asn1.obj" \
    "$(INTDIR)\gss_debug.obj" \
    "$(INTDIR)\gss_ext.obj" \
    "$(INTDIR)\gss_impl.obj" \
    "$(INTDIR)\gss_oidtbl.obj" \
    "$(INTDIR)\gss_util.obj" \
    "$(INTDIR)\gss_x500_ns.obj" \
    "$(INTDIR)\nil_scheme.obj" \
    "$(INTDIR)\rfc822_ns.obj"

"$(OUTDIR)\gss.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

OUTDIR=$(ILUHOME)\bin
INTDIR=$(ILUSRC)\GSS\kernel\WinDebug
# Begin Custom Macros
OutDir=$(ILUHOME)\bin
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\gss.dll"

!ELSE 

ALL : "$(OUTDIR)\gss.dll"

!ENDIF 

CLEAN :
    -@erase "$(INTDIR)\anon_ns.obj"
    -@erase "$(INTDIR)\gss_asn1.obj"
    -@erase "$(INTDIR)\gss_debug.obj"
    -@erase "$(INTDIR)\gss_ext.obj"
    -@erase "$(INTDIR)\gss_impl.obj"
    -@erase "$(INTDIR)\gss_oidtbl.obj"
    -@erase "$(INTDIR)\gss_util.obj"
    -@erase "$(INTDIR)\gss_x500_ns.obj"
    -@erase "$(INTDIR)\nil_scheme.obj"
    -@erase "$(INTDIR)\rfc822_ns.obj"
    -@erase "$(INTDIR)\vc50.idb"
    -@erase "$(INTDIR)\vc50.pdb"
    -@erase "$(OUTDIR)\gss.dll"
    -@erase "$(OUTDIR)\gss.exp"
    -@erase "$(OUTDIR)\gss.ilk"
    -@erase "$(ILUHOME)\lib\gss.lib"
    -@erase "$(OUTDIR)\gss.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "$(ILUSRC)\GSS\kernel" /D\
 "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "GSS_BUILDING_KERNEL" /Fp"$(INTDIR)\gss.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=$(ILUSRC)\GSS\kernel\WinDebug/
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
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\gss.bsc" 
BSC32_SBRS= \
    
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /dll /incremental:yes\
 /pdb:"$(OUTDIR)\gss.pdb" /debug /machine:I386 /out:"$(OUTDIR)\gss.dll"\
 /implib:"$(ILUHOME)\lib\gss.lib" /pdbtype:sept 
LINK32_OBJS= \
    "$(INTDIR)\anon_ns.obj" \
    "$(INTDIR)\gss_asn1.obj" \
    "$(INTDIR)\gss_debug.obj" \
    "$(INTDIR)\gss_ext.obj" \
    "$(INTDIR)\gss_impl.obj" \
    "$(INTDIR)\gss_oidtbl.obj" \
    "$(INTDIR)\gss_util.obj" \
    "$(INTDIR)\gss_x500_ns.obj" \
    "$(INTDIR)\nil_scheme.obj" \
    "$(INTDIR)\rfc822_ns.obj"

"$(OUTDIR)\gss.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(CFG)" == "Win32 Release" || "$(CFG)" == "Win32 Debug"
SOURCE=$(ILUSRC)\GSS\kernel\anon_ns.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_ANON_=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_ext.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_impl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_namespace.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_scheme.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    "$(ILUSRC)\GSS\kernel\ilugssns_anonymous.h"\
    

"$(INTDIR)\anon_ns.obj" : $(SOURCE) $(DEP_CPP_ANON_) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_ANON_=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_ext.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_impl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_namespace.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_scheme.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    "$(ILUSRC)\GSS\kernel\ilugssns_anonymous.h"\
    

"$(INTDIR)\anon_ns.obj" : $(SOURCE) $(DEP_CPP_ANON_) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\GSS\kernel\gss_asn1.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GSS_A=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_asn1.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_ext.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_impl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_namespace.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_scheme.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    

"$(INTDIR)\gss_asn1.obj" : $(SOURCE) $(DEP_CPP_GSS_A) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GSS_A=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_asn1.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_ext.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_impl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_namespace.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_scheme.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    

"$(INTDIR)\gss_asn1.obj" : $(SOURCE) $(DEP_CPP_GSS_A) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\GSS\kernel\gss_debug.c
DEP_CPP_GSS_D=\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    

"$(INTDIR)\gss_debug.obj" : $(SOURCE) $(DEP_CPP_GSS_D) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=$(ILUSRC)\GSS\kernel\gss_ext.c
DEP_CPP_GSS_E=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_asn1.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    

"$(INTDIR)\gss_ext.obj" : $(SOURCE) $(DEP_CPP_GSS_E) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=$(ILUSRC)\GSS\kernel\gss_impl.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GSS_I=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_asn1.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_ext.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_impl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_namespace.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_scheme.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    

"$(INTDIR)\gss_impl.obj" : $(SOURCE) $(DEP_CPP_GSS_I) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GSS_I=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_asn1.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_ext.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_impl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_namespace.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_scheme.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    

"$(INTDIR)\gss_impl.obj" : $(SOURCE) $(DEP_CPP_GSS_I) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\GSS\kernel\gss_oidtbl.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GSS_O=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    

"$(INTDIR)\gss_oidtbl.obj" : $(SOURCE) $(DEP_CPP_GSS_O) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GSS_O=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    

"$(INTDIR)\gss_oidtbl.obj" : $(SOURCE) $(DEP_CPP_GSS_O) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\GSS\kernel\gss_util.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GSS_U=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    

"$(INTDIR)\gss_util.obj" : $(SOURCE) $(DEP_CPP_GSS_U) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GSS_U=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    

"$(INTDIR)\gss_util.obj" : $(SOURCE) $(DEP_CPP_GSS_U) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\GSS\kernel\gss_x500_ns.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_GSS_X=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_ext.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_impl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_namespace.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_scheme.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_x500_ns.h"\
    

"$(INTDIR)\gss_x500_ns.obj" : $(SOURCE) $(DEP_CPP_GSS_X) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_GSS_X=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_ext.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_impl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_namespace.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_scheme.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_x500_ns.h"\
    

"$(INTDIR)\gss_x500_ns.obj" : $(SOURCE) $(DEP_CPP_GSS_X) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\GSS\kernel\nil_scheme.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_NIL_S=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_ext.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_impl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_namespace.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_scheme.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    "$(ILUSRC)\GSS\kernel\ilugssmech_nil.h"\
    "$(ILUSRC)\GSS\kernel\ilugssns_anonymous.h"\
    

"$(INTDIR)\nil_scheme.obj" : $(SOURCE) $(DEP_CPP_NIL_S) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_NIL_S=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_ext.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_impl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_namespace.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_scheme.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    "$(ILUSRC)\GSS\kernel\ilugssmech_nil.h"\
    "$(ILUSRC)\GSS\kernel\ilugssns_anonymous.h"\
    

"$(INTDIR)\nil_scheme.obj" : $(SOURCE) $(DEP_CPP_NIL_S) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=$(ILUSRC)\GSS\kernel\rfc822_ns.c

!IF  "$(CFG)" == "Win32 Release"

DEP_CPP_RFC82=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_ext.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_impl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_namespace.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_scheme.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    "$(ILUSRC)\GSS\kernel\ilugssns_rfc822.h"\
    

"$(INTDIR)\rfc822_ns.obj" : $(SOURCE) $(DEP_CPP_RFC82) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Win32 Debug"

DEP_CPP_RFC82=\
    "$(ILUSRC)\GSS\kernel\gssapi.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_conf.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_debug.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_ext.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_impl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_namespace.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_oidtbl.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_opaque.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_scheme.h"\
    "$(ILUSRC)\GSS\kernel\ilugss_util.h"\
    "$(ILUSRC)\GSS\kernel\ilugssns_rfc822.h"\
    

"$(INTDIR)\rfc822_ns.obj" : $(SOURCE) $(DEP_CPP_RFC82) "$(INTDIR)"
    $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

