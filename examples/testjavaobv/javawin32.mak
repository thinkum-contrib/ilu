# Top level makefile to build Win32 java version of ILU testjavaobv example

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

!IF "$(JAVASDK)" == ""
!ERROR JAVASDK environment variable not set - should be one of Microsoft or Javasoft
!ENDIF 

!IF "$(JDK_HOME)" == ""
JDK_HOME = $(JAVA_HOME)
!ENDIF

JAVA_CLASS_DIR=.\classes
JAVA_SPECIALCLASS_DIR=.\specialclasses
JAVA_STUBS_DIR=.\javastubs


!IF "$(CFG)" == "Win32 Release"

!IF "$(JAVASDK)" == "Microsoft"
ILU_JAVAC_FLAGS= /d $(JAVA_CLASS_DIR)
ILU_SPECIALJAVAC_FLAGS= /d $(JAVA_SPECIALCLASS_DIR)
!ELSE
ILU_JAVAC_FLAGS= -d $(JAVA_CLASS_DIR)
ILU_SPECIALJAVAC_FLAGS= -d $(JAVA_SPECIALCLASS_DIR)
!ENDIF

!ELSE

!IF "$(JAVASDK)" == "Microsoft"
ILU_JAVAC_FLAGS= /g /d $(JAVA_CLASS_DIR)
ILU_SPECIALJAVAC_FLAGS= /g /d $(JAVA_SPECIALCLASS_DIR)
!ELSE
ILU_JAVAC_FLAGS= -g -d $(JAVA_CLASS_DIR)
ILU_SPECIALJAVAC_FLAGS= -g- d $(JAVA_SPECIALCLASS_DIR)
!ENDIF

!ENDIF


!IF "$(JAVASDK)" == "Microsoft"
JAVAC = $(JDK_HOME)\bin\jvc
!ELSE
JAVAC = $(JDK_HOME)\bin\javac
!ENDIF

!IF "$(JAVASDK)" == "Microsoft"
ALL : NOJAVAJAR
!ELSE
ALL : JAVAMAKE
!ENDIF

!ENDIF

################################################################################


NOJAVAJAR :
    echo testjavaobv example requires 'jar', not available with Microsoft JAVASDK
    
NOJAVAMAKE :
    echo JAVA_HOME environment variable not set, not building Java example


JAVAMAKE :
    java-stubber testjavaserialobjects.isl
    if not exist $(JAVA_CLASS_DIR)\nul mkdir $(JAVA_CLASS_DIR)
    if not exist $(JAVA_SPECIALCLASS_DIR)\nul mkdir $(JAVA_SPECIALCLASS_DIR)
    set CLASSPATH=.;$(JAVA_CLASS_DIR);$(ILUHOME)\lib\classes;$(ILUHOME)\lib\ilujavaobv.jar;$(CLASSPATH)
    $(JAVAC) $(ILU_JAVAC_FLAGS) $(JAVA_STUBS_DIR)\testjavaserialobjects\*.java  
    $(JAVAC) $(ILU_JAVAC_FLAGS) TestReceiving.java
    $(JAVAC) $(ILU_SPECIALJAVAC_FLAGS) TestSending.java FooWhizBangFuzzler.java
    cd $(JAVA_SPECIALCLASS_DIR)
    jar cv0f ../MyJarFile.jar *; 
    cd ..
    


clean :
    if exist $(JAVA_CLASS_DIR)\nul rmdir $(JAVA_CLASS_DIR) /s
    if exist $(JAVA_STUBS_DIR)\nul rmdir $(JAVA_STUBS_DIR) /s
    if exist $(JAVA_SPECIALCLASS_DIR)\nul rmdir $(JAVA_SPECIALCLASS_DIR) /s
    

# End 
################################################################################
