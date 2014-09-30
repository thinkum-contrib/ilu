# Top level makefile to build Win 32 version of ILU examples using
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

SUBDIRECTORIES= timeit.dir test1.dir test2.dir httest.dir iiop.dir tutorial.dir \
	cpp2foo.dir pickle.dir javatest1.dir ngtest.dir 

CLEANSUBS= timeit.clean test1.clean test2.clean httest.clean iiop.clean tutorial.clean \
	cpp2foo.clean pickle.clean javatest1.clean ngtest.clean


################################################################################

ALL : $(SUBDIRECTORIES)

$(SUBDIRECTORIES) :
	cd $*
	nmake -f iluwin32.mak CFG="$(CFG)"
	cd ..

clean : $(CLEANSUBS)

$(CLEANSUBS) :
	cd $*
	nmake -f iluwin32.mak CFG="$(CFG)" clean
	cd ..

 
# End 
################################################################################
