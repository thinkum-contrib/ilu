
#
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
##   File:       islscan.make#   Target:     islscan#   Sources:    iluparser.c malloc.c pathname.c scan.c SHS.c type_uid.c#   Created:    Monday, August 2, 1993 4:52:48 PMOBJECTS = ¶		iluparser.c.o ¶		malloc.c.o ¶		pathname.c.o ¶		SHS.c.o ¶		type_uid.c.o ¶		simplebcopy.c.oSCANOBJ = scan.c.oCOptions = -r -model far -sym on -mbg full -d YYDEBUG -d MACOSislscan ÄÄ islscan.make isl-parser {SCANOBJ}	Link -model far -sym on -d -c 'MPS ' -t MPST ¶		{SCANOBJ} ¶		isl-parser	¶		"{CLibraries}"CSANELib.o ¶		"{CLibraries}"Math.o ¶		#"{CLibraries}"Complex.o ¶		"{CLibraries}"StdClib.o ¶		"{Libraries}"Stubs.o ¶		"{Libraries}"Runtime.o ¶		"{Libraries}"Interface.o ¶		"{Libraries}"ToolLibs.o ¶		-o islscan		isl-parser	ÄÄ islscan.make {OBJECTS}	Lib {OBJECTS} -o 'isl-parser'iluparser.c	Ä	ilu.bison	Bison -o iluparser.c ilu.bison
