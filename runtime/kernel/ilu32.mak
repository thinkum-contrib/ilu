# Modified so that $(ILUHOME) can be used to specifiy location
# Note that if you use this as a VC++ make file, $(ILUHOME) wiil be \ilu
# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **
# $Id: ilu32.mak,v 1.31 1998/10/06 20:20:46 larner Exp $

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "ilu32.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
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
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRel"
# PROP Intermediate_Dir "WinRel"
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : $(ILUHOME)\bin\ilu32.dll $(OUTDIR)/ilu32.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Ox /Oy- /I "\ilu\src\runtime\kernel" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "SUNRPC_PROTOCOL" /D "TCPIP_TRANSPORT" /D "UDPSOCKET_TRANSPORT" /DREGISTRY_LAST_RESORT="\"\\ilu\\registry\"" /DILU_BINDING_DIRECTORY="\"\\ilu\\bindings\""  /c
# SUBTRACT CPP /Ot /Og /Oi /Os
# Note we used to use the /Ox /Oy- optimization, but starting with MCVS5 and ilu2.0alpha9
# the global optimization started producing bogus code
CPP_PROJ=/nologo /MD /W3 /GX /Ob1 /Oi /Ot /Gs /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /I "$(ILUSRC)\common\patchlevel" \
 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "SUNRPC_PROTOCOL" /D "TCPIP_TRANSPORT" /D\
 "UDPSOCKET_TRANSPORT" /Fp$(OUTDIR)/"ilu32.pch" /Fo$(INTDIR)/\
 /c /D "ILU_BUILDING_KERNEL"
CPP_OBJS=.\WinRel/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
    
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"ilu32.bsc" 

$(OUTDIR)/ilu32.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386
# ADD LINK32 wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386 /OUT:"\ilu\bin\ilu32.dll"
LINK32_FLAGS=$(ILUHOME)\lib\gss.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /INCREMENTAL:no\
 /PDB:$(OUTDIR)/"ilu32.pdb" /MACHINE:I386 \
 /OUT:"$(ILUHOME)\bin\ilu32.dll" /IMPLIB:"$(ILUHOME)\lib\ilu32.lib" 
LINK32_OBJS= \
    $(INTDIR)/alarmux.obj \
    $(INTDIR)/bsdmnlp.obj \
    $(INTDIR)/bsdutils.obj \
    $(INTDIR)/call.obj \
    $(INTDIR)/connect.obj \
    $(INTDIR)/courier.obj \
    $(INTDIR)/crc32.obj \
    $(INTDIR)/debug.obj \
    $(INTDIR)/error.obj \
    $(INTDIR)/gc.obj \
    $(INTDIR)/gss.obj \
    $(INTDIR)/hash.obj \
    $(INTDIR)/http.obj \
    $(INTDIR)/identity.obj \
    $(INTDIR)/iiop.obj \
    $(INTDIR)/ilubufxp.obj \
    $(INTDIR)/inmem.obj \
    $(INTDIR)/locks.obj \
    $(INTDIR)/mainloop.obj \
    $(INTDIR)/memory.obj \
    $(INTDIR)/method.obj \
    $(INTDIR)/newtcp.obj \
    $(INTDIR)/object.obj \
    $(INTDIR)/port.obj \
    $(INTDIR)/iluprotocol.obj \
    $(INTDIR)/sbfile.obj \
    $(INTDIR)/server.obj \
    $(INTDIR)/sunrpc.obj \
    $(INTDIR)/sunrpcrm.obj \
    $(INTDIR)/threads.obj \
    $(INTDIR)/ilutransport.obj \
    $(INTDIR)/type.obj \
    $(INTDIR)/udp.obj \
    $(INTDIR)/iluvector.obj \
    $(INTDIR)/win32.obj \
    $(INTDIR)/wsock.obj \
    $(INTDIR)/types.obj \
    $(INTDIR)/pickle.obj \
    $(INTDIR)/pickle2.obj \
    $(INTDIR)/pickle3.obj \
    $(INTDIR)/w3ng.obj \
    $(INTDIR)/w3mux.obj \
    $(INTDIR)/ilutpcod.obj

    

$(ILUHOME)\bin\ilu32.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

ALL : $(ILUHOME)\bin\ilu32.dll $(OUTDIR)/ilu32.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Od /I "\ilu\src\runtime\kernel" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "SUNRPC_PROTOCOL" /D "TCPIP_TRANSPORT" /D "UDPSOCKET_TRANSPORT" /DREGISTRY_LAST_RESORT="\"\\ilu\\registry\"" /DILU_BINDING_DIRECTORY="\"\\ilu\\bindings\""  /c
CPP_PROJ=/nologo /MD /W3 /GX /Zi /Od /I "$(ILUSRC)\runtime\kernel" /I "$(ILUSRC)\GSS\kernel" /I "$(ILUSRC)\common\patchlevel" \
 /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "SUNRPC_PROTOCOL" /D "TCPIP_TRANSPORT" /D\
 "UDPSOCKET_TRANSPORT" /Fp$(OUTDIR)/"ilu32.pch" /Fo$(INTDIR)/\
 /Fd$(OUTDIR)/"ilu32.pdb" \
 /c /D "ILU_BUILDING_KERNEL"  $(ILU_DEBUG_CFLAGS)
CPP_OBJS=.\WinDebug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
    
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"ilu32.bsc" 

$(OUTDIR)/ilu32.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386
# ADD LINK32 wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386 /OUT:"\ilu\bin\ilu32.dll" /IMPLIB:"\ilu\lib\ilu32.lib"
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS= $(ILUHOME)\lib\gss.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /INCREMENTAL:yes\
 /PDB:$(OUTDIR)/"ilu32.pdb" /DEBUG /MACHINE:I386 \
 /OUT:"$(ILUHOME)\bin\ilu32.dll" /IMPLIB:"$(ILUHOME)\lib\ilu32.lib" 
LINK32_OBJS= \
    $(INTDIR)/alarmux.obj \
    $(INTDIR)/bsdmnlp.obj \
    $(INTDIR)/bsdutils.obj \
    $(INTDIR)/call.obj \
    $(INTDIR)/connect.obj \
    $(INTDIR)/courier.obj \
    $(INTDIR)/crc32.obj \
    $(INTDIR)/debug.obj \
    $(INTDIR)/error.obj \
    $(INTDIR)/gc.obj \
    $(INTDIR)/gss.obj \
    $(INTDIR)/hash.obj \
    $(INTDIR)/http.obj \
    $(INTDIR)/identity.obj \
    $(INTDIR)/iiop.obj \
    $(INTDIR)/ilubufxp.obj \
    $(INTDIR)/inmem.obj \
    $(INTDIR)/locks.obj \
    $(INTDIR)/mainloop.obj \
    $(INTDIR)/memory.obj \
    $(INTDIR)/method.obj \
    $(INTDIR)/newtcp.obj \
    $(INTDIR)/object.obj \
    $(INTDIR)/port.obj \
    $(INTDIR)/iluprotocol.obj \
    $(INTDIR)/sbfile.obj \
    $(INTDIR)/server.obj \
    $(INTDIR)/sunrpc.obj \
    $(INTDIR)/sunrpcrm.obj \
    $(INTDIR)/ilutransport.obj \
    $(INTDIR)/threads.obj \
    $(INTDIR)/type.obj \
    $(INTDIR)/udp.obj \
    $(INTDIR)/iluvector.obj \
    $(INTDIR)/win32.obj \
    $(INTDIR)/wsock.obj \
    $(INTDIR)/types.obj \
    $(INTDIR)/pickle.obj \
    $(INTDIR)/pickle2.obj \
    $(INTDIR)/pickle3.obj \
    $(INTDIR)/w3ng.obj \
    $(INTDIR)/w3mux.obj \
    $(INTDIR)/ilutpcod.obj

$(ILUHOME)\bin\ilu32.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=.\alarmux.c

$(INTDIR)/alarmux.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\bsdmnlp.c
DEP_BSDMN=\
    .\iluntrnl.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/bsdmnlp.obj :  $(SOURCE)  $(DEP_BSDMN) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\bsdutils.c
DEP_BSDUT=\
    .\iluntrnl.h\
    .\oscalls.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\os\posix.h\
    .\os\win.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/bsdutils.obj :  $(SOURCE)  $(DEP_BSDUT) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\call.c
DEP_CALL_=\
    .\iluntrnl.h\
    .\iluprotocol.h\
    .\connect.h\
    .\ilutransport.h\
    .\port.h\
    .\call.h\
    .\object.h\
    .\method.h\
    .\server.h\
    .\type.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\iluerror.h\
    .\iluvector.h\
    .\ilubasic.h

$(INTDIR)/call.obj :  $(SOURCE)  $(DEP_CALL_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\connect.c

$(INTDIR)/connect.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\courier.c

$(INTDIR)/courier.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\crc32.c

$(INTDIR)/crc32.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\debug.c

$(INTDIR)/debug.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\error.c

$(INTDIR)/error.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\gc.c
DEP_GC_C6=\
    .\iluntrnl.h\
    .\object.h\
    .\call.h\
    .\server.h\
    .\type.h\
    .\iluvector.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/gc.obj :  $(SOURCE)  $(DEP_GC_C6) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\gss.c
DEP_GSS_C6=\
    .\iluntrnl.h\
    .\ilutransport.h\
    .\mooring.h\
    .\ilusock.h\
    .\oscalls.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\os\posix.h\
    .\os\win.h\
    .\iluerror.h\
    .\ilubasic.h\
    ..\..\GSS\kernel\gssapi.h


$(INTDIR)/gss.obj :  $(SOURCE)  $(DEP_GSS_C6) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\hash.c
DEP_HASH_=\
    .\iluhash.h

$(INTDIR)/hash.obj :  $(SOURCE)  $(DEP_HASH_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\http.c
DEP_HASH_=\
    .\http.h\
    .\iluntrnl.h\
    .\call.h\
    .\connect.h\
    .\ilutransport.h\
    .\object.h\
    .\method.h\
    .\server.h\
    .\type.h\
    .\iluprotocol.h

$(INTDIR)/htpprot.obj :  $(SOURCE)  $(DEP_HASH_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\w3ng.c
DEP_HASH_=\
    .\w3ng.h\
    .\iluntrnl.h\
    .\call.h\
    .\connect.h\
    .\ilutransport.h\
    .\object.h\
    .\method.h\
    .\server.h\
    .\type.h\
    .\iluprotocol.h

$(INTDIR)/w3ng.obj :  $(SOURCE)  $(DEP_HASH_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\w3mux.c
DEP_HASH_=\
    .\iluntrnl.h\
    .\ilutransport.h\
    .\iluvector.h\
    .\mooring.h

$(INTDIR)/w3mux.obj :  $(SOURCE)  $(DEP_HASH_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\identity.c

$(INTDIR)/identity.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ilubufxp.c

$(INTDIR)/ilubufxp.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\iiop.c

$(INTDIR)/iiop.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\inmem.c

$(INTDIR)/inmem.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\locks.c

$(INTDIR)/locks.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mainloop.c
DEP_MAINL=\
    .\iluntrnl.h\
    .\oscalls.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\os\posix.h\
    .\os\win.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/mainloop.obj :  $(SOURCE)  $(DEP_MAINL) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\memory.c
DEP_MEMOR=\
    .\iluntrnl.h\
    .\oscalls.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\os\posix.h\
    .\os\win.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/memory.obj :  $(SOURCE)  $(DEP_MEMOR) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\method.c
DEP_METHO=\
    .\iluntrnl.h\
    .\method.h\
    .\type.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/method.obj :  $(SOURCE)  $(DEP_METHO) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\newtcp.c
DEP_TCP_C=\
    .\iluntrnl.h\
    .\ilutransport.h\
    .\mooring.h\
    .\ilusock.h\
    .\oscalls.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\os\posix.h\
    .\os\win.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/newtcp.obj :  $(SOURCE)  $(DEP_TCP_C) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\object.c

$(INTDIR)/object.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\port.c

$(INTDIR)/port.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\iluprotocol.c

$(INTDIR)/iluprotocol.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\server.c
DEP_SERVE=\
    .\iluntrnl.h\
    .\server.h\
    .\object.h\
    .\type.h\
    .\connect.h\
    .\port.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/server.obj :  $(SOURCE)  $(DEP_SERVE) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\sbfile.c
DEP_SIMPB=\
    .\iluntrnl.h\
    .\object.h\
    .\oscalls.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\os\posix.h\
    .\os\win.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/sbfile.obj :  $(SOURCE)  $(DEP_SIMPB) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\sunrpc.c
DEP_SUNRP=\
    .\iluntrnl.h\
    .\packet.h\
    .\sunrpc.h\
    .\call.h\
    .\iluprotocol.h\
    .\connect.h\
    .\ilutransport.h\
    .\port.h\
    .\object.h\
    .\type.h\
    .\method.h\
    .\mooring.h\
    .\os\posix.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/sunrpc.obj :  $(SOURCE)  $(DEP_SUNRP) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\sunrpcrm.c

$(INTDIR)/sunrpcrm.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\threads.c
DEP_THRD=\
    .\threads.h\
    .\iluntrnl.h\
    .\oscalls.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\os\posix.h\
    .\os\win.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/threads.obj :  $(SOURCE)  $(DEP_THRD) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ilutransport.c

$(INTDIR)/ilutransport.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\type.c
DEP_TYPE_=\
    .\iluntrnl.h\
    .\object.h\
    .\server.h\
    .\type.h\
    .\iluvector.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/type.obj :  $(SOURCE)  $(DEP_TYPE_) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\udp.c
DEP_UDP_C=\
    .\iluntrnl.h\
    .\ilutransport.h\
    .\mooring.h\
    .\ilusock.h\
    .\oscalls.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\os\posix.h\
    .\os\win.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/udp.obj :  $(SOURCE)  $(DEP_UDP_C) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\iluvector.c
DEP_VECTO=\
    .\iluntrnl.h\
    .\iluvector.h\
    .\iluxport.h\
    .\ilutpcod.h\
    .\iluerrs.h\
    .\iludebug.h\
    .\iluhash.h\
    .\iluerror.h\
    .\ilubasic.h

$(INTDIR)/iluvector.obj :  $(SOURCE)  $(DEP_VECTO) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\win32.c

$(INTDIR)/win32.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wsock.c

$(INTDIR)/wsock.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\types.c
DEP_VECTO=\
    .\iluntrnl.h\
    .\ilutypes.h\
    .\call.h\
    .\connect.h\
    .\iluprotocol.h\
    .\iluvector.h\
    .\iluerror.h\
    .\ilubasic.h\
    .\iluxport.h\
    .\ilutpcod.h    

$(INTDIR)/types.obj :  $(SOURCE)  $(DEP_VECTO) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\pickle.c
DEP_VECTO=\
    .\iluntrnl.h\
    .\ilutypes.h\
    .\call.h\
    .\connect.h\
    .\iluprotocol.h\
    .\iluerror.h\
    .\ilubasic.h\
    .\iluxport.h\
    .\ilutpcod.h    

$(INTDIR)/pickle.obj :  $(SOURCE)  $(DEP_VECTO) $(INTDIR)

################################################################################
# Begin Source File

SOURCE=.\pickle2.c
DEP_VECTO=\
    .\iluntrnl.h\
    .\ilutypes.h\
    .\call.h\
    .\connect.h\
    .\iluprotocol.h\
    .\iluerror.h\
    .\ilubasic.h\
    .\iluxport.h\
    .\ilutpcod.h    

$(INTDIR)/pickle2.obj :  $(SOURCE)  $(DEP_VECTO) $(INTDIR)

################################################################################
# Begin Source File

SOURCE=.\pickle3.c
DEP_VECTO=\
    .\iluntrnl.h\
    .\ilutypes.h\
    .\call.h\
    .\connect.h\
    .\iluprotocol.h\
    .\iluerror.h\
    .\ilubasic.h\
    .\iluxport.h\
    .\ilutpcod.h    

$(INTDIR)/pickle3.obj :  $(SOURCE)  $(DEP_VECTO) $(INTDIR)

################################################################################
# Begin Source File

SOURCE=.\ilutpcod.c
DEP_VECTO=\
    .\iluntrnl.h\
    .\iluerror.h\
    .\ilubasic.h\
    .\iluxport.h\
    .\ilutpcod.h    

$(INTDIR)/ilutpcod.obj :  $(SOURCE)  $(DEP_VECTO) $(INTDIR)

# End Source File
# End Group
# End Project
################################################################################

