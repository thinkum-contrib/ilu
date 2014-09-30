# BeginILUCopyright
# 
# Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.
# 
# Unlimited use, reproduction, modification, and distribution of this
# software and modified versions thereof is permitted.  Permission is
# granted to make derivative works from this software or a modified
# version thereof.  Any copy of this software, a modified version
# thereof, or a derivative work must include both the above copyright
# notice of Xerox Corporation and this paragraph.  Any distribution of
# this software, a modified version thereof, or a derivative work must
# comply with all applicable United States export control laws.  This
# software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
# WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
# LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
# EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
# NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGES.
#   
# EndILUCopyright
# 

all:: Build

BootstrapMakefile: ./imake/configure
	@echo '******************************************************************************'
	@echo '***  Configuring with defaults.                                            ***'
	@echo '***  If you need something different, interrupt this process and           ***'
	@echo '***   type "./configure --help" to see other options.                      ***'
	@echo '***  Or read the top level README file.                                    ***'
	@echo '******************************************************************************'
	cd ./imake ; ./configure

ConfiguredMakefile:  BootstrapMakefile ./imake/ilu.defs.new ./imake/ilu.tmpl ./imake/ilu.rules
	make -f BootstrapMakefile ConfiguredMakefile

.makefiles: ConfiguredMakefile
	make -f ConfiguredMakefile Makefiles
	touch .makefiles

Build: .makefiles
	make -f ConfiguredMakefile

Install: Build
	make -f ConfiguredMakefile Install

Clean::
	make -f ConfiguredMakefile Clean

Dist:: .makefiles
	make -f ConfiguredMakefile Dist RELNUM=$(RELNUM)
