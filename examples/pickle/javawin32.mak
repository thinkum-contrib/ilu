# Top level makefile to build Win32 java version of ILU pickle example

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
JAVAC = $(JDK_HOME)\bin\jvc
!ELSE
JAVAC = $(JDK_HOME)\bin\javac
!ENDIF

ALL : JAVAMAKE

!ENDIF

################################################################################


NOJAVAMAKE :
	echo JAVA_HOME environment variable not set, not building Java example


JAVAMAKE :
	java-stubber Test1.isl Test2.isl
	if not exist $(JAVA_CLASS_DIR)\nul mkdir $(JAVA_CLASS_DIR)
	set CLASSPATH=.;$(JAVA_CLASS_DIR);$(ILUHOME)\lib\classes;$(CLASSPATH)
	$(JAVAC) $(ILU_JAVAC_FLAGS) $(JAVA_STUBS_DIR)\Test1\*.java $(JAVA_STUBS_DIR)\Test2\*.java
	$(JAVAC) $(ILU_JAVAC_FLAGS) *.java


clean :
	if exist $(JAVA_CLASS_DIR)\nul rmdir $(JAVA_CLASS_DIR) /s
	if exist $(JAVA_STUBS_DIR)\nul rmdir $(JAVA_STUBS_DIR) /s
	

# End 
################################################################################
