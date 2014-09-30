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
$Id: util.h,v 1.17 1999/08/03 01:50:34 janssen Exp $
*/

typedef iluparser_EnumProc	EnumProc;

extern char *		programName;
extern Interface	currentIfc;
extern boolean		generatingSkeleton;

extern void		sysFatal(const char *);
extern void		fatal(const char *, ...);

extern boolean		isPrefixOf(const char *prefix, const char *base);
extern boolean		matchPointer(void *p1, void *p2);
extern boolean		matchString(void *p1, void *p2);

extern char *		booleanImage(int value);
extern TypeDescription	baseTypeDescription(Type t);
extern char *		simpleTypeName(Type t);
extern char *		arraySpecialElemTypeName(Type t);
extern char *		sequenceSpecialElemTypeName(Type t);
extern int		methodResultCount(Procedure m);

extern char *		sol;
extern void		indent(int levelDelta);
extern void		newline(void);

extern void		printBanner(const char *part, Interface ifc);
extern void		printImportIfc(const char *ifcName, boolean skelToo);
extern void		printImportTable(void);
extern void		printTypeRef(Type t);
extern void		sprintTypeRef(char *buf, Type t);
extern void		printArgList(list argList, int nPrevArgs);
extern void		printClassVarName(Type t, const char *varName);
extern void		printFullExceptionName(Exception e);
extern void		printExceptionIDString(Exception e);
extern void		printTypeIoFuncName(Type t, const char *prefix);
extern void		printNameScopes(list scopes);

extern void		modifyIndentLevel(int /* change */);
extern void		setFileAndLine(char *, int);
extern int		indentedPrintf(const char * /* formatSpec */, ...);

#ifdef ILU_CORBA_PYTHON_MAPPING
#define printf indentedPrintf
#endif
