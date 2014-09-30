# Top level makefile to build Win32 java version of ILU javaobv feature

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
ALL : NOJAVAJAR
!ELSE
ALL : JAVAMAKE
!ENDIF

!ENDIF

################################################################################


NOJAVAJAR :
    echo javaobv requires 'jar', not available with Microsoft JAVASDK
    
NOJAVAMAKE :
    echo JAVA_HOME environment variable not set, not building javaobv


JAVAMAKE :
    set ILUPATH=.;..\..\stubbers\parser
    java-stubber ilujava.isl
    if not exist $(JAVA_CLASS_DIR)\nul mkdir $(JAVA_CLASS_DIR)
    set CLASSPATH=.;$(JAVA_CLASS_DIR);$(ILUHOME)\lib\classes;..\..\runtime\java\classes;$(CLASSPATH)
    $(JAVAC) $(ILU_JAVAC_FLAGS) $(JAVA_STUBS_DIR)\xerox\ilujava\*.java
    $(JAVAC) $(ILU_JAVAC_FLAGS) jsrc\xerox\ilujava\IluOBVSetter.java jsrc\xerox\ilujava\IluOBVClassLoaderGetter.java jsrc\xerox\ilujava\IluOBVClassLoaderSetter.java jsrc\xerox\ilujava\IluOBV.java jsrc\xerox\ilujava\Holder.java jsrc\xerox\ilujava\IluOBVWireClassLoader.java jsrc\xerox\ilujava\ClassAccessorImplFromJar.java
    cd .\classes
    $(JDK_HOME)\bin\jar -0cf ..\ilujavaobv.jar *
    cd ..
#   When know how to distinguish between 1.2 and 1.1.x, can conditionally add this line for 1.2
#   before the jar file creation of course!
#   $(JAVAC) $(ILU_JAVAC_FLAGS) IluOBVURLClassLoader.java
    


clean :
    if exist $(JAVA_CLASS_DIR)\nul rmdir $(JAVA_CLASS_DIR) /s
    if exist $(JAVA_STUBS_DIR)\nul rmdir $(JAVA_STUBS_DIR) /s
    if exist ilujavaobv.jar del ilujavaobv.jar

# End 
################################################################################
