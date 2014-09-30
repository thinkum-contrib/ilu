# Top level makefile to build Win32 java version of ILU CosNaming feature

!IF "$(CFG)" == ""
CFG=Win32 Release
!MESSAGE No configuration specified.  Defaulting to Win32 Release.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "javawin32.mak" CFG="Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release"
!MESSAGE "Win32 Debug"
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 


!IF "$(JAVA_HOME)" == ""
ALL : NOJAVAMAKE
!ELSE
!IF "$(JDK_HOME)" == ""
JDK_HOME = $(JAVA_HOME)
!ENDIF

!IF "$(JAVASDK)" == ""
!ERROR JAVASDK environment variable not set - should be one of Microsoft or Javasoft
!ENDIF 

JAVA_CLASS_DIR=.\classes
JAVA_STUBS_DIR=.\javastubs


!IF "$(CFG)" == "Win32 Release"

!IF "$(JAVASDK)" == "Microsoft"
ILU_JAVAC_FLAGS= /d $(JAVA_CLASS_DIR)
!ELSE
ILU_JAVAC_FLAGS= -d $(JAVA_CLASS_DIR)
!ENDIF

!ELSE

!IF "$(JAVASDK)" == "Microsoft"
ILU_JAVAC_FLAGS= /g /d $(JAVA_CLASS_DIR)
!ELSE
ILU_JAVAC_FLAGS= -g -d $(JAVA_CLASS_DIR)
!ENDIF

!ENDIF


!IF "$(JAVASDK)" == "Microsoft"
JAVAC = $(JAVA_HOME)\bin\jvc
!ELSE
JAVAC = $(JDK_HOME)\bin\javac
!ENDIF

!IF "$(JAVASDK)" == "Microsoft"
ALL : JAVAMAKE 
!ELSE
ALL : JAVAMAKE JAVAJAR
!ENDIF

!ENDIF

################################################################################


JAVAJAR :
    cd .\classes
    $(JDK_HOME)\bin\jar -0cf ..\iluCosNaming.jar *
    cd .. 
    
NOJAVAMAKE :
    echo JAVA_HOME environment variable not set, not building Java CosNaming


JAVAMAKE :
    set ILUPATH=.;..\..\stubbers\parser
    java-stubber -prefix xorg.omg CosNaming.idl
    if not exist $(JAVA_CLASS_DIR)\nul mkdir $(JAVA_CLASS_DIR)
    set CLASSPATH=.;$(JAVA_CLASS_DIR);$(ILUHOME)\lib\classes;..\..\runtime\java\classes;$(CLASSPATH)
    $(JAVAC) $(ILU_JAVAC_FLAGS) $(JAVA_STUBS_DIR)\xorg\omg\CosNaming\*.java $(JAVA_STUBS_DIR)\xorg\omg\CosNaming\NamingContextPackage\*.java  $(JAVA_STUBS_DIR)\xorg\omg\CosNaming\NamingContextExtPackage\*.java
    


clean :
    if exist $(JAVA_CLASS_DIR)\nul rmdir $(JAVA_CLASS_DIR) /s
    if exist $(JAVA_STUBS_DIR)\nul rmdir $(JAVA_STUBS_DIR) /s
    if exist iluCosNaming.jar del iluCosNaming.jar

# End 
################################################################################
