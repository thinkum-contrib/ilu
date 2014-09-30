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
#File:       'c++-stubber.make'#   Target:     'c++-stubber'#   Sources:    client.c#               code.c#               common.c#               cplusplus.c#               declare-object.c#               headers.c#               names.c#               server.c#   Created:    Monday, August 9, 1993 7:27:09 PMOBJECTS = ¶		client.c.o ¶		code.c.o ¶		common.c.o ¶		cplusplus.c.o ¶		declare-object.c.o ¶		headers.c.o ¶		names.c.o ¶		server.c.oCOptions = -r -model far -sym on -i ::'ISL parser': -mbg full -d YYDEBUG -d MACOS'c++-stubber' ÄÄ 'c++-stubber.make' {OBJECTS}	Link -model far -sym on -d -c 'MPS ' -t MPST ¶		{OBJECTS} ¶		::'ISL parser':isl-parser	¶		"{CLibraries}"CSANELib.o ¶		"{CLibraries}"Math.o ¶		#"{CLibraries}"Complex.o ¶		"{CLibraries}"StdClib.o ¶		"{Libraries}"Runtime.o ¶		"{Libraries}"Interface.o ¶		-o 'c++-stubber'
