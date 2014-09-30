# Top level makefile to build Win32 version of ILU using
# Microsoft Visual C++

JAVA_SRC_DIR = jsrc
ILU_CLASSPATHARG =


!IF "$(CFG)" == ""
CFG=Win32 Release
!MESSAGE No configuration specified.  Defaulting to Win32 Release.
!ENDIF 


!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "iluwin32.mak" CFG="Win32 Release"
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

JAVA_CLASS_DIR=.\classes
JAVA_BUILD_DIR=.\build


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

ILU_GNH_FLAGS= -rni
HSTUB_OLD=

!ELSE

!IF "$(JAVAUSE12)" != ""
!IF "$(JAVAUSEJNI)" != ""
ILU_GNH_FLAGS= -jni
HSTUB_OLD=
!ELSE
ILU_GNH_FLAGS= -oni
HSTUB_OLD= -old
!ENDIF
!ELSE
ILU_GNH_FLAGS= -oni
HSTUB_OLD=
!ENDIF

!ENDIF



!IF "$(JAVASDK)" == "Microsoft"
!MESSAGE Building for Microsoft SDK
JAVAC = $(JDK_HOME)\bin\jvc
JAVAH = $(JDK_HOME)\bin\msjavah
JAVAI = $(JAVA_HOME)\bin\jview
ILU_JAVAH_FLAGS= /d $(JAVA_BUILD_DIR) /noclinit
ILU_JAVAH_STUB_FLAG =
ILU_JAVARTMAKE = ilumsoft.mak
ILU_JAVA_CONFIGFILE=ilujava_conf.h.ms2
ALL : $(ILUHOME) $(ILUHOME)\bin $(ILUHOME)\lib JAVAMAKE

!ELSE

JAVAC= $(JDK_HOME)\bin\javac
JAVAH= $(JDK_HOME)\bin\javah
JAVAI= $(JAVA_HOME)\bin\java
ILU_JAVAH_FLAGS= -d $(JAVA_BUILD_DIR)
ILU_JAVAH_STUB_FLAG = -stubs
!IF "$(JAVAUSE12)" != ""
!IF "$(JAVAUSEJNI)" != ""
!MESSAGE Building for Javasoft JDK-1.2 with JNI
ILU_JAVARTMAKE= ilujsoft12jni.mak
!ELSE
!MESSAGE Building for Javasoft JDK-1.2 with ONI
ILU_JAVARTMAKE= ilujsoft12.mak
!ENDIF
ILU_JAVA_CONFIGFILE=ilujava_conf.h.12
!ELSE
!MESSAGE Building for Javasoft JDK-1.1
ILU_JAVARTMAKE= ilujsoft.mak
ILU_JAVA_CONFIGFILE=ilujava_conf.h.11
!ENDIF
ALL : $(ILUHOME) $(ILUHOME)\bin $(ILUHOME)\lib JAVAMAKE JAVAGMAKE
!ENDIF


!ENDIF

################################################################################

NOJAVAMAKE :
    echo JAVA_HOME environment variable not set, not building Java runtime

$(ILUHOME) : 
    if not exist $(ILUHOME)\nul mkdir $(ILUHOME)

$(ILUHOME)\lib : 
    if not exist $(ILUHOME)\lib\nul mkdir $(ILUHOME)\lib

$(ILUHOME)\bin : 
    if not exist $(ILUHOME)\bin\nul mkdir $(ILUHOME)\bin

BASIC_SOURCES = \
    $(JAVA_SRC_DIR)\xerox\basics\NowhereStream.java \
    $(JAVA_SRC_DIR)\xerox\basics\NowhereWriter.java \
    $(JAVA_SRC_DIR)\xerox\basics\Consumer0.java \
    $(JAVA_SRC_DIR)\xerox\basics\Queue.java \
    $(JAVA_SRC_DIR)\xerox\basics\Environment.java \
    $(JAVA_SRC_DIR)\xerox\basics\VMExtras.java \
    $(JAVA_SRC_DIR)\xerox\basics\IntTab.java \
    $(JAVA_SRC_DIR)\xerox\ilu\float128.java

PRE_SOURCES = \
    $(JAVA_SRC_DIR)\xerox\ilu\IluInit.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluMozillaBase.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluPreLoad.java

OMG_OMGSRCDIR = omgsrc\org\omg

OMG_SOURCES = \
    $(OMG_OMGSRCDIR)\CORBA\TypeCodePackage\BadKind.java \
    $(OMG_OMGSRCDIR)\CORBA\TypeCodePackage\Bounds.java \
    $(OMG_OMGSRCDIR)\CORBA\Bounds.java \
    $(OMG_OMGSRCDIR)\CORBA\ORBPackage\InvalidName.java \
    $(OMG_OMGSRCDIR)\CORBA\portable\Streamable.java \
    $(OMG_OMGSRCDIR)\CORBA\portable\OutputStream.java \
    $(OMG_OMGSRCDIR)\CORBA\portable\InputStream.java \
    $(OMG_OMGSRCDIR)\CORBA\SetOverrideType.java \
    $(OMG_OMGSRCDIR)\CORBA\OperationDef.java \
    $(OMG_OMGSRCDIR)\CORBA\Principal.java \
    $(OMG_OMGSRCDIR)\CORBA\WrongTransaction.java \
    $(OMG_OMGSRCDIR)\CORBA\Environment.java \
    $(OMG_OMGSRCDIR)\CORBA\ContextList.java \
    $(OMG_OMGSRCDIR)\CORBA\ExceptionList.java \
    $(OMG_OMGSRCDIR)\CORBA\IDLType.java \
    $(OMG_OMGSRCDIR)\CORBA\StructMember.java \
    $(OMG_OMGSRCDIR)\CORBA\UnionMember.java \
    $(OMG_OMGSRCDIR)\CORBA\ImplementationDef.java \
    $(OMG_OMGSRCDIR)\CORBA\InterfaceDef.java \
    $(OMG_OMGSRCDIR)\CORBA\Request.java \
    $(OMG_OMGSRCDIR)\CORBA\NamedValue.java \
    $(OMG_OMGSRCDIR)\CORBA\NVList.java \
    $(OMG_OMGSRCDIR)\CORBA\Context.java \
    $(OMG_OMGSRCDIR)\CORBA\CompletionStatus.java \
    $(OMG_OMGSRCDIR)\CORBA\NO_MEMORY.java \
    $(OMG_OMGSRCDIR)\CORBA\IMP_LIMIT.java \
    $(OMG_OMGSRCDIR)\CORBA\COMM_FAILURE.java \
    $(OMG_OMGSRCDIR)\CORBA\INITIALIZE.java \
    $(OMG_OMGSRCDIR)\CORBA\NO_IMPLEMENT.java \
    $(OMG_OMGSRCDIR)\CORBA\BAD_TYPECODE.java \
    $(OMG_OMGSRCDIR)\CORBA\NO_RESOURCES.java \
    $(OMG_OMGSRCDIR)\CORBA\NO_RESPONSE.java \
    $(OMG_OMGSRCDIR)\CORBA\PERSIST_STORE.java \
    $(OMG_OMGSRCDIR)\CORBA\BAD_INV_ORDER.java \
    $(OMG_OMGSRCDIR)\CORBA\TRANSIENT.java \
    $(OMG_OMGSRCDIR)\CORBA\FREE_MEM.java \
    $(OMG_OMGSRCDIR)\CORBA\INV_IDENT.java \
    $(OMG_OMGSRCDIR)\CORBA\INV_FLAG.java \
    $(OMG_OMGSRCDIR)\CORBA\INTF_REPOS.java \
    $(OMG_OMGSRCDIR)\CORBA\BAD_CONTEXT.java \
    $(OMG_OMGSRCDIR)\CORBA\OBJ_ADAPTER.java \
    $(OMG_OMGSRCDIR)\CORBA\OBJECT_NOT_EXIST.java \
    $(OMG_OMGSRCDIR)\CORBA\TRANSACTIONREQUIRED.java \
    $(OMG_OMGSRCDIR)\CORBA\TRANSACTIONROLLEDBACK.java \
    $(OMG_OMGSRCDIR)\CORBA\INTERNAL.java \
    $(OMG_OMGSRCDIR)\CORBA\MARSHAL.java \
    $(OMG_OMGSRCDIR)\CORBA\BAD_PARAM.java \
    $(OMG_OMGSRCDIR)\CORBA\UNKNOWN.java \
    $(OMG_OMGSRCDIR)\CORBA\SystemException.java \
    $(OMG_OMGSRCDIR)\PortableServer\POA.java \
    $(OMG_OMGSRCDIR)\PortableServer\DynamicImplementation.java \
    $(OMG_OMGSRCDIR)\CORBA\UserException.java \
    $(OMG_OMGSRCDIR)\CORBA\BAD_OPERATION.java \
    $(OMG_OMGSRCDIR)\CORBA\DATA_CONVERSION.java \
    $(OMG_OMGSRCDIR)\CORBA\NO_PERMISSION.java \
    $(OMG_OMGSRCDIR)\CORBA\INV_OBJREF.java \
    $(OMG_OMGSRCDIR)\CORBA\TCKind.java \
    $(OMG_OMGSRCDIR)\CORBA\BOA.java \
    $(OMG_OMGSRCDIR)\CORBA\ORB.java \
    $(OMG_OMGSRCDIR)\CORBA\Object.java \
    $(OMG_OMGSRCDIR)\CORBA\DomainManager.java \
    $(OMG_OMGSRCDIR)\CORBA\Policy.java \
    $(OMG_OMGSRCDIR)\CORBA\portable\ObjectImpl.java \
    $(OMG_OMGSRCDIR)\CORBA\portable\Delegate.java \
    $(OMG_OMGSRCDIR)\CORBA\TypeCode.java \
    $(OMG_OMGSRCDIR)\CORBA\Any.java 

PRE_EXCEPTION_SOURCES = \
    $(JAVA_SRC_DIR)\xerox\ilu\IluInconsistentCallException.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluUnexpectedException.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluNotConfiguredException.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluGcRegFailedException.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluSystemExceptionBase.java

RT_SOURCES = \
    $(JAVA_SRC_DIR)\xerox\ilu\IluConstantValueKind.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluTypeKind.java \
    $(JAVA_SRC_DIR)\xerox\ilu\CStringHolder.java \
    $(JAVA_SRC_DIR)\xerox\ilu\CStringHelper.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluLifetimeArgs.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluLifetimeRemember.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluLifetimeForget.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluUserException.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluDebugWriter.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluEnvironment.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluDebug.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluDataSupport.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluIHProposer.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluObject.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluWPBase.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluObjectBase.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluFactory.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluPassport.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluPipeline.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluSerializationContext.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluIdentity.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluIdentityType.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluSurrogateConnection.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluServerConnection.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluServiceThread.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluInit2.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluDebugHooks.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluClassRep.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluClassAccess.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluRuntimeException.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluExceptionRep.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluOInt.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluAlarmTech.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluForkTech.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluCall.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluMethodRep.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluMethodArgRep.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluServer.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluServerRelocation.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluServerRelocationInfo.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluObjectTable.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluRT0.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluSkeleton.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluSurrogateObject.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluTransportInfo.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluPort.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluBatcher.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluGssCred.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluGssOid.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluServantFinalizable.java \
    $(JAVA_SRC_DIR)\xerox\ilu\corba_ServantFinalizer.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluGCClient.java \
    $(JAVA_SRC_DIR)\xerox\ilu\Ilu.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluSBH.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluResolving.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluSubstitute.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluSimpleBinding.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluTypeCode.java \
    $(JAVA_BUILD_DIR)\IluTypeIdProps.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluTypeRep.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluAny.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluORB.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluIOFunctions.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluCustomMappingException.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluCustomMapping.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluPickle.java \
    $(JAVA_SRC_DIR)\xerox\ilu\IluRootObjectHelper.java \
    $(JAVA_SRC_DIR)\xerox\ilu\CORBA_ObjectStub.java \
    $(JAVA_SRC_DIR)\xerox\ilu\CORBA_ObjectHelper.java \
    $(JAVA_SRC_DIR)\xerox\ilu\CORBA_WStringHelper.java

HOLDER_SOURCES = \
    $(OMG_OMGSRCDIR)\CORBA\ShortHolder.java \
    $(OMG_OMGSRCDIR)\CORBA\IntHolder.java \
    $(OMG_OMGSRCDIR)\CORBA\LongHolder.java \
    $(OMG_OMGSRCDIR)\CORBA\ByteHolder.java \
    $(OMG_OMGSRCDIR)\CORBA\FloatHolder.java \
    $(OMG_OMGSRCDIR)\CORBA\DoubleHolder.java \
    $(OMG_OMGSRCDIR)\CORBA\CharHolder.java \
    $(OMG_OMGSRCDIR)\CORBA\BooleanHolder.java \
    $(OMG_OMGSRCDIR)\CORBA\StringHolder.java \
    $(OMG_OMGSRCDIR)\CORBA\ObjectHolder.java \
    $(OMG_OMGSRCDIR)\CORBA\AnyHolder.java \
    $(OMG_OMGSRCDIR)\CORBA\TypeCodeHolder.java \
    $(OMG_OMGSRCDIR)\CORBA\PrincipalHolder.java 

XEROX_C_AND_H_SOURCES = \
    xerox.ilu.IluInit2 \
    xerox.ilu.IluPassport \
    xerox.ilu.IluSerializationContext \
    xerox.ilu.IluPipeline \
    xerox.ilu.IluIdentity \
    xerox.ilu.IluIdentityType \
    xerox.ilu.IluServiceThread \
    xerox.ilu.IluServerConnection \
    xerox.ilu.IluSurrogateConnection \
    xerox.ilu.IluOInt \
    xerox.ilu.IluClassRep \
    xerox.ilu.IluServer \
    xerox.ilu.IluServerRelocation \
    xerox.ilu.IluServerRelocationInfo \
    xerox.ilu.IluAlarmTech \
    xerox.ilu.IluForkTech \
    xerox.ilu.IluCall \
    xerox.ilu.IluExceptionRep \
    xerox.ilu.IluRT0 \
    xerox.ilu.IluTransportInfo \
    xerox.ilu.IluPort \
    xerox.ilu.IluBatcher \
    xerox.ilu.IluGssCred \
    xerox.ilu.IluGssOid \
    xerox.ilu.IluDebugHooks \
    xerox.ilu.IluGCClient \
    xerox.ilu.IluDebug \
    xerox.ilu.IluDebugWriter \
    xerox.ilu.IluPickle \
    xerox.ilu.IluTypeCode \
    xerox.ilu.IluTypeRep \
    xerox.ilu.IluSBH \
    xerox.ilu.IluWPBase


XEROX_H_SOURCES = \
    xerox.ilu.IluObject \
    xerox.ilu.IluMethodRep \
    xerox.ilu.IluMethodArgRep \
    xerox.ilu.IluObjectTable \
    xerox.ilu.IluSurrogateObject


JAVAMAKE :
    if not exist .\classes\nul mkdir .\classes
    if not exist .\build\nul mkdir .\build
    if exist .\ilunative.h attrib -r .\ilunative.h
    if exist .\ilunative.c attrib -r .\ilunative.c
    if exist .\config\ilujava_conf.h attrib -r .\config\ilujava_conf.h
    copy $(ILU_JAVA_CONFIGFILE) .\config\ilujava_conf.h
    $(JAVAC) $(ILU_JAVAC_FLAGS) $(JAVA_SRC_DIR)\xerox\ilutools\gnh.java
    $(JAVAC) $(ILU_JAVAC_FLAGS) $(JAVA_SRC_DIR)\xerox\ilutools\japp.java
    $(JAVAC) $(ILU_JAVAC_FLAGS) $(JAVA_SRC_DIR)\xerox\ilutools\GenIluTypeIds.java
    set CLASSPATH=.\classes;$(CLASSPATH)
    $(JAVAI) $(ILU_CLASSPATHARG) xerox.ilutools.gnh $(ILU_GNH_FLAGS) ilunative.in 
    $(ILUHOME)\bin\islscan -I ../../stubbers/parser build.isl >$(JAVA_BUILD_DIR)\typeids.scanned 
    $(JAVAI) $(ILU_CLASSPATHARG) xerox.ilutools.GenIluTypeIds 
!IF "$(JAVAUSE12)" == ""
!MESSAGE Compiling the omg classes
    $(JAVAC) $(ILU_CLASSPATHARG) $(ILU_JAVAC_FLAGS) $(OMG_SOURCES)
    $(JAVAC) $(ILU_CLASSPATHARG) $(ILU_JAVAC_FLAGS) $(HOLDER_SOURCES)
!ELSE
!ENDIF
    $(JAVAC) $(ILU_CLASSPATHARG) $(ILU_JAVAC_FLAGS) $(BASIC_SOURCES)
    $(JAVAC) $(ILU_CLASSPATHARG) $(ILU_JAVAC_FLAGS) $(PRE_SOURCES)
    $(JAVAC) $(ILU_CLASSPATHARG) $(ILU_JAVAC_FLAGS) $(PRE_EXCEPTION_SOURCES)
    $(JAVAC) $(ILU_CLASSPATHARG) $(ILU_JAVAC_FLAGS) $(RT_SOURCES)
!IF "$(JAVAUSEJNI)" == ""
    $(JAVAH) $(ILU_CLASSPATHARG) $(HSTUB_OLD) $(ILU_JAVAH_FLAGS) $(ILU_JAVAH_STUB_FLAG) $(XEROX_C_AND_H_SOURCES)
!ENDIF
    $(JAVAH) $(ILU_CLASSPATHARG) $(HSTUB_OLD) $(ILU_JAVAH_FLAGS) $(XEROX_C_AND_H_SOURCES)
    $(JAVAH) $(ILU_CLASSPATHARG) $(HSTUB_OLD) $(ILU_JAVAH_FLAGS) $(XEROX_H_SOURCES)
    nmake -f $(ILU_JAVARTMAKE) CFG="$(CFG)"
    
# for javasoft, also make the _g version    
JAVAGMAKE : JAVAMAKE
    if exist $(JAVA_HOME)\lib\javai_g.lib nmake -f $(ILU_JAVARTMAKE) CFG="$(CFG)" ILUJG="_g"


clean :
    if exist WinDebug\nul del /q WinDebug\*
    if exist WinDebug\nul rmdir WinDebug
    if exist WinRel\nul del /q Winrel\*
    if exist WinRel\nul rmdir Winrel
    if exist $(JAVA_CLASS_DIR) rmdir $(JAVA_CLASS_DIR) /s
    if exist $(JAVA_BUILD_DIR) rmdir $(JAVA_BUILD_DIR) /s
    

# End 
################################################################################
