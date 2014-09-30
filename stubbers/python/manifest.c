/** 
 BeginILUCopyright

 Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.

 Unlimited use, reproduction, modification, and distribution of this
 software and modified versions thereof is permitted.  Permission is
 granted to make derivative works from this software or a modified
 version thereof.  Any copy of this software, a modified version
 thereof, or a derivative work must include both the above copyright
 notice of Xerox Corporation and this paragraph.  Any distribution of
 this software, a modified version thereof, or a derivative work must
 comply with all applicable United States export control laws.  This
 software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
 WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
 LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
 EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
 NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
 OF THE POSSIBILITY OF SUCH DAMAGES.
  
 EndILUCopyright
*/

/*
$Id: manifest.c,v 1.24 1999/09/02 06:07:55 janssen Exp $
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iluptype.h>
#include "manifest.h"

const char	fmtEnumImageDict[]	= "imageOf___%s";
const char	fmtEnumLit[]		= "%s__%s";
const char	fmtFileName[]		= "%s.py";
const char	fmtFuncIo[]		= "_%s_%s";
const char	fmtFuncSkel[]		= "_%s__%s";
#ifdef ILU_CORBA_PYTHON_MAPPING
const char	fmtSkelModuleName[]	= "%s__POA";
#else
const char	fmtSkelModuleName[]	= "%s__skel";
#endif
const char	fmtStubModuleName[]	= "%s";
const char	fmtUnionDiscrimLit[]	= "%s__%s";

const char	prefixIdlAttribute[]	= "ilu--prefix-idlAttribute-";
const char	prefixIdlExcType[]	= "ilu--prefix-idlExceptionType-";

const char	prefixFuncEnd[]		= "End";
const char	prefixFuncInput[]	= "Input";
const char	prefixFuncOutput[]	= "Output";
const char	prefixFuncSizeOf[]	= "SizeOf";

const char	nameClassRecord[]	= "IluRecord";
const char	nameClassSkel[]		= "IluObjTrue";
const char	nameClassStub[]		= "IluObjSurr";
const char	nameClassUnion[]	= "IluUnion";
const char	nameExceptRetry[]	= "RetryCall";
const char	nameExceptUnimpl[]	= "IluUnimplementedMethodError";
const char	nameFuncCatchExcept[]	= "_CatchException";
const char	nameFuncSendExcept[]	= "_SendException";
const char	nameFuncCacheAccess[]	= "IluGetCachedValue";
const char	nameFuncCacheValue[]	= "IluSetCachedValue";
const char	nameLocalStub[]		= "IluObjLocal";
const char	nameModuleCORBA[]	= "CORBA";
const char	nameModuleIlu[]		= "iluRt";
const char	nameModuleTypes[]	= "types";
const char	nameSystemException[]	= "SystemException";
const char	nameUserException[]	= "UserException";
const char	nameVarCache[]		= "__cached_values__";
const char	nameVarTemp[]		= "_tmp";
const char	nameVarCall[]		= "_call";
const char	nameVarType[]		= "_type";
const char	nameVarClass[]		= "_IluClass";
const char	nameVarDiscrim[]	= "_d";
const char	nameVarExceptCode[]	= "_ecode";
const char	nameVarExceptName[]	= "_name";
const char	nameVarExceptValue[]	= "_value";
const char	nameVarImports[]	= "_imported_modules";
const char	nameVarTypeUID[]	= "_uid";
const char	nameVarValueValue[]	= "_value";
const char	nameVarIndex[]		= "_i";
const char	nameVarLength[]		= "_length";
const char	nameVarMstid[]		= "mstid";
const char	nameVarResult[]		= "_result";
const char	nameVarSbh[]		= "sbh";
const char	nameVarSelf[]		= "_self";
const char	nameVarSize[]		= "_size";
const char	nameVarValue[]		= "_value";

const char	suffixIdlExcType[]	= "__omgidl_exctype";
