# Top level makefile to build Win32 version of ILU
# Microsoft Visual C++
#
# Assumes your PATH has the appropriate bin directory in it (e.g. msvc\bin)
# Assumes your INCLUDE environment variable includes the appropriate directories (e.g. msvc\include) 
# Assumes your LIB environment variable includes the appropriate directories (e.g. msvc\lib) 

!IF "$(CFG)" == ""
CFG=Win32 Release
!MESSAGE No configuration specified.  Defaulting to Win32 Release.
!MESSAGE Possible choices for configuration are: "Win32 Release" "Win32 Debug"
!ENDIF 


################################################################################

SUBDIRECTORIES= GSS.dir stubbers.dir runtime.dir etc.dir
CLEANSUBS= GSS.clean stubbers.clean runtime.clean etc.clean

################################################################################

ALL : $(ILUHOME) $(ILUHOME)\bin $(ILUHOME)\lib $(SUBDIRECTORIES)

$(ILUHOME) : 
    if not exist $(ILUHOME)\nul mkdir $(ILUHOME)

$(ILUHOME)\lib : 
    if not exist $(ILUHOME)\lib\nul mkdir $(ILUHOME)\lib

$(ILUHOME)\bin : 
    if not exist $(ILUHOME)\bin\nul mkdir $(ILUHOME)\bin

$(SUBDIRECTORIES) :
    cd $*
    nmake -f iluwin32.mak CFG="$(CFG)"
    cd ..

clean : $(CLEANSUBS)

$(CLEANSUBS) :
    cd $*
    nmake -f iluwin32.mak CFG="$(CFG)" clean
    cd ..


install : $(ILUHOME)\installdirs $(ILUHOME)\interfacefiles $(ILUHOME)\includefiles $(ILUHOME)\examplefiles $(ILUHOME)\pythonfiles $(ILUHOME)\javafiles  CLEANOUT 

$(ILUHOME)\installdirs : 
    if not exist $(ILUHOME)\interfaces\nul mkdir $(ILUHOME)\interfaces
    if not exist $(ILUHOME)\include\nul mkdir $(ILUHOME)\include
    if not exist $(ILUHOME)\examples\nul mkdir $(ILUHOME)\examples
    if not exist $(ILUHOME)\lib\classes\nul mkdir $(ILUHOME)\lib\classes


$(ILUHOME)\interfacefiles : $(ILUHOME)\interfaces
    xcopy $(ILUSRC)\stubbers\parser\ilu.isl $(ILUHOME)\interfaces /d
    xcopy $(ILUSRC)\stubbers\parser\iluhttp.isl $(ILUHOME)\interfaces /d
    xcopy $(ILUSRC)\etc\CosNaming\CosNaming.idl $(ILUHOME)\interfaces /d


$(ILUHOME)\includefiles :   $(ILUHOME)\include
    xcopy $(ILUSRC)\runtime\cpp\ilu.hh $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\ilubasic.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\c\iluchdrs.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\c\ilucstub.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\iludebug.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\iluerror.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\iluerrs.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\iluhash.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\iluntrnl.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\stubbers\parser\iluptype.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\iluxport.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\ilusock.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\iluwin.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\ilutypes.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\iluprotocol.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\ilutransport.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\iluvector.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\kernel\ilutpcod.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\cpp2\ilu.hpp $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\cpp2\corba.hpp $(ILUHOME)\include /d
    xcopy $(ILUSRC)\runtime\cpp2\corba-templates.hpp $(ILUHOME)\include /d
    xcopy $(ILUSRC)\stubbers\cpp2\cppportability.hpp $(ILUHOME)\include /d
    xcopy $(ILUSRC)\GSS\kernel\gssapi.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\GSS\kernel\ilugsswin_conf.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\GSS\kernel\ilugssmech_nil.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\GSS\kernel\ilugssns_anonymous.h $(ILUHOME)\include /d
    xcopy $(ILUSRC)\GSS\kernel\ilugssns_rfc822.h $(ILUHOME)\include /d

$(ILUHOME)\examplefiles :   $(ILUHOME)\examples
    xcopy $(ILUSRC)\examples $(ILUHOME)\examples /s /d 

$(ILUHOME)\pythonfiles :    $(ILUHOME)\lib
    xcopy $(ILUSRC)\runtime\python\*.py $(ILUHOME)\lib /d


$(ILUHOME)\javafiles :  $(ILUHOME)\lib
    if exist $(ILUSRC)\runtime\java\classes xcopy $(ILUSRC)\runtime\java\classes $(ILUHOME)\lib\classes /s /d 
    if exist $(ILUSRC)\etc\javaobv\ilujavaobv.jar copy $(ILUSRC)\etc\javaobv\ilujavaobv.jar $(ILUHOME)\lib 
    if exist $(ILUSRC)\etc\javaobv\ilujava.isl copy $(ILUSRC)\etc\javaobv\ilujava.isl $(ILUHOME)\interfaces
    if exist $(ILUSRC)\etc\javaobv\ilujava.map copy $(ILUSRC)\etc\javaobv\ilujava.map $(ILUHOME)\interfaces
    if exist $(ILUSRC)\etc\CosNaming\classes xcopy $(ILUSRC)\etc\CosNaming\classes $(ILUHOME)\lib\classes /s /d 
    if exist $(ILUSRC)\etc\CosNaming\iluCosNaming.jar copy $(ILUSRC)\etc\CosNaming\iluCosNaming.jar $(ILUHOME)\lib 


CLEANOUT : 
    if exist $(ILUHOME)\bin del /q $(ILUHOME)\bin\*.ilk
    if exist $(ILUHOME)\bin del /q $(ILUHOME)\bin\*.exp
    if exist $(ILUHOME)\bin del /q $(ILUHOME)\bin\*.map
    if exist $(ILUHOME)\lib del /q $(ILUHOME)\lib\*.ilk
    if exist $(ILUHOME)\lib del /q $(ILUHOME)\lib\*.exp
    if exist $(ILUHOME)\lib del /q $(ILUHOME)\lib\*.map

 
# End 
################################################################################
