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
# $Id: ilu.py,v 1.48 1999/08/03 01:55:33 janssen Exp $
#

# Exceptions
from iluRt import       \
    IluGeneralError,    \
    IluProtocolError,   \
    IluUnimplementedMethodError

# Constants
from iluRt import       \
    FALSE,		\
    TRUE,		\
    FineTimeRate,       \
    Version,            \
    CORBAMapping,	\
    DictionaryPassing,	\
    BuildDomain,	\
    TypeUIDVersion

# Methods
from iluRt import       \
    SetCalloutExceptionHandler, \
    SetDebugLevel,      \
    SetDebugLevelViaString, \
    CheckStubConsistency,   \
    CRC32,		\
    CreateServer,       \
    DefaultServer,      \
    Server,		\
    ObjectOfSBH,        \
    LookupObject,       \
    ParseSBH,           \
    FormSBH,            \
    FindObject,         \
    FindOrCreateSurrogate,  \
    RegisterCustomSurrogate,    \
    IOROfObject,        \
    CallerIdentity,     \
    RegisterInputHandler,   \
    RegisterOutputHandler,   \
    CreateLoopHandle,       \
    RunMainLoop,        \
    ExitMainLoop,       \
    SetMainLoop,        \
    DoSoon,         \
    LongReal,           \
    FineTime,           \
    FineTime_Now,       \
    CreateAlarm,                \
    CreatePipeline,     \
    SetPipeline,        \
    GetPipeline,        \
    SetSerializer,      \
    GetSerializer,      \
    SetPassport,        \
    GetPassport,        \
    CreatePassport,     \
    Pickle,         \
    Typecode,           \
    ThreadedOperation,      \
    SetFDBudget,        \
    GetFDBudget,        \
    TCPStatistics,      \
    TCPDefaultBufferSize,   \
    CORBA_Object,       \
    __types__

if CORBAMapping:
	from iluRt import RaiseSystemException

import iluRt
if iluRt.__dict__.has_key("AcquireGSSCredForName"):
	from iluRt import AcquireGSSCredForName
del iluRt

def UnregisterInputHandler (fd):
    RegisterInputHandler (fd, None)

def TypeName(cl):
    return cl._IluClass.name()

def TypeID(cl):
    return cl._IluClass.id()

def AutoImport(path=(), verbose=None):
    import iluimport
    iluimport.install(path, verbose)

import os
if not os.environ.has_key("ILU_PYTHON_DISABLE_AUTOIMPORT"):
	AutoImport()
