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
$Id: name.h,v 1.9 1999/08/03 01:50:33 janssen Exp $
*/

extern char *		getArgumentName(Argument a);
extern char *		getConstantName(Constant c);
extern char *		getEnumFieldName(EnumField e, char *buffer);
extern char *		getExceptionName(Exception e);
extern char *		getSimpleExceptionName(Exception e);
extern char *		getImportName(const char *name, char *buffer);
extern char *		getInterfaceName(Interface i);
extern char *		getSimpleInterfaceName(Interface i);
extern char *		getProcedureName(Procedure p);
extern char *		getTypeName(Type t);
extern char *		getSimpleTypeName(Type t);
extern char *		getScopedTypeName(Type t);
extern char *		getTypeUID(Type t);
extern char *		getIslProcedureName(Procedure p);
