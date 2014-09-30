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
$Id: util.c,v 1.32 1999/09/02 06:08:59 janssen Exp $
*/

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "iluptype.h"
#include "manifest.h"
#include "name.h"
#include "util.h"

#include <version.h>

char *		programName;
Interface	currentIfc;
boolean		generatingSkeleton;

void
sysFatal(const char *msg)
{
	fprintf(stderr, "Fatal error: ");
	perror(msg);
	exit(1);
}

void
fatal(const char *fmt, ...)
{
	va_list	args;

	va_start(args, fmt);
	fprintf(stderr, "Fatal error: ");
	vfprintf(stderr, fmt, args);
	va_end(args);
	putc('\n', stderr);
	exit(1);
}

/************************************************************************/

boolean
isPrefixOf(const char *prefix, const char *base)
{
	int	i;

	for (i = 0; prefix[i]; i++)
		if (prefix[i] != base[i])
			return FALSE;
	return TRUE;
}

boolean
matchPointer(void *p1, void *p2)
{
  return (p1 == p2);
}

boolean
matchString(void *p1, void *p2)
{
  return (strcmp((char *) p1, (char *) p2) == 0);
}

/************************************************************************/

char *
booleanImage(int value)
{
	return value ? "TRUE" : "FALSE";
}

TypeDescription
baseTypeDescription(Type t)
{
	Type	base	= t;

	for (;;)
	{
		if (base->description)
			return base->description;
		base = base->supertype;
	}
}

char *
simpleTypeName(Type t)
{
	TypeDescription	d	= baseTypeDescription(t);

	switch (d->type)
	{
	case boolean_Type:		return "Boolean";
	case byte_Type:			return "Byte";
	case cardinal_Type:		return "Cardinal";
	case character_Type:		return "Character";
	case enumeration_Type:		return "Enum";
	case integer_Type:		return "Integer";
	case longcardinal_Type:		return "LongCardinal";
	case longinteger_Type:		return "LongInteger";
	case longreal_Type:		return "LongReal";
	case real_Type:			return "Real";
	case shortcardinal_Type:	return "ShortCardinal";
	case shortcharacter_Type:	return "Byte";
	case shortinteger_Type:		return "ShortInteger";
	case shortreal_Type:		return "ShortReal";
	case string_Type:		return "String";
	case pickle_Type:		return "Pickle";
	default:			return 0;
	}
}

char *
arraySpecialElemTypeName(Type t)
{
	switch (baseTypeDescription(t)->type)
	{
	case byte_Type:			return "Opaque";
	case shortcharacter_Type:	return "StringVec";
	case character_Type:		return "WStringVec";
	default:			return 0;
	}
}

char *
sequenceSpecialElemTypeName(Type t)
{
	switch (baseTypeDescription(t)->type)
	{
	case byte_Type:			return "Bytes";
	case shortcharacter_Type:	return "String";
	case character_Type:		return "WString";
	default:			return 0;
	}
}

static void
countResultArg(Argument a, int *pCount)
{
	if (a->direction == Out || a->direction == InOut)
		*pCount += 1;
}

int
methodResultCount(Procedure m)
{
	int	count	= m->returnType != 0;

	list_enumerate(m->arguments, (EnumProc) countResultArg, &count);
	return count;
}

/************************************************************************/

static char	spacing[] = "                                                ";
char *		sol = spacing + sizeof spacing - 1;

void
indent(int levelDelta)
{
	sol -= levelDelta * 2;
	if (sol < spacing || spacing + sizeof spacing <= sol)
		fatal("internal error: indentation");
}

void
newline(void)
{
  printf ("\n");
}

/************************************************************************/

void
printBanner(const char *part, Interface ifc)
{
	static char *prefixesForPython[2] = { "#", "#" };

	printf("# %s for \"%s\"\n", part, interface_name(ifc));
	printf("#\n");
	iluparser_GenerateBoilerplate(stdout, ifc, programName,
		prefixesForPython);
	printf("\n");
}

void
printImportIfc(const char *ifcName, boolean skelToo)
{
	if (strcmp(ifcName, "ilu") == 0)
		printf("import %s\n", nameModuleIlu);
	else
	{
		char	buffer[1024], modulename[2048];
		char *	name	= getImportName(ifcName, buffer);

		printf("import %s; %s['%s'] = %s; del %s;",
		       name, nameVarImports, name, name, name);
		if (skelToo)
		{
		  newline();
		  sprintf (modulename, fmtSkelModuleName, name);
		  printf("import %s; %s['%s'] = %s; del %s;",
			 modulename, nameVarImports, modulename, modulename, modulename);
		}
		newline();
	}
}

void
printImportTable (void)
{
  printf ("%s = {};\n", nameVarImports);
  printf ("import %s ; %s['%s'] = %s; %s['ilu'] = %s;\n",
	  nameModuleIlu, nameVarImports, nameModuleIlu, nameModuleIlu, nameVarImports, nameModuleIlu);
#ifdef ILU_CORBA_PYTHON_MAPPING
  printf ("%s.CheckStubConsistency2('%s', '%s', %s.TRUE);\n", nameModuleIlu,
	  ILU_VERSION_STRING, ILU_TYPEUID_VERSION_STRING, nameModuleIlu);
  printf ("%s['ilu__POA'] = %s\n", nameVarImports, nameModuleIlu);
#else
  printf ("%s.CheckStubConsistency('%s', '%s');\n", nameModuleIlu,
	  ILU_VERSION_STRING, ILU_TYPEUID_VERSION_STRING);
  printf ("%s['ilu__skel'] = %s\n", nameVarImports, nameModuleIlu);
#endif /* def ILU_CORBA_PYTHON_MAPPING */
  printf ("del %s\n", nameModuleIlu);
#ifdef ILU_CORBA_PYTHON_MAPPING
  printf ("import %s; %s['%s'] = %s; del %s;\n",
	  nameModuleCORBA, nameVarImports, nameModuleCORBA, nameModuleCORBA, nameModuleCORBA);
  printf ("import %s; %s['%s'] = %s; del %s;\n",
	  nameModuleTypes, nameVarImports, nameModuleTypes, nameModuleTypes, nameModuleTypes);
#endif /* def ILU_CORBA_PYTHON_MAPPING */
}

/************************************************************************/

static void
printArg(Argument a, int *pCount)
{
	if (a->direction == In || a->direction == InOut)
		printf("%s%s", ++*pCount > 1 ? ", " : "", getArgumentName(a));
}

void
printArgList(list argList, int nPrevArgs)
{
	int	count	= nPrevArgs;

	list_enumerate(argList, (EnumProc) printArg, &count);
}

static void
printIfcPrefix(Interface ifc)
{
	if (generatingSkeleton || ifc != currentIfc)
		printf("%s.", getInterfaceName(ifc));
}

static void
sprintIfcPrefix(char *buf, Interface ifc)
{
	if (generatingSkeleton || ifc != currentIfc)
		sprintf(buf, "%s.", getInterfaceName(ifc));
}

void
printClassVarName(Type t, const char *varName)
{
	Type	urt	= ur_type(t);

	printIfcPrefix(urt->interface);
	printf("%s", getTypeName(urt));
	if (varName)
		printf(".%s", varName);
}

void
printFullExceptionName(Exception e)
{
	printIfcPrefix((e->importInterfaceName == ILU_NIL) ? e->interface : e->import->interface);
	printf("%s", getSimpleExceptionName(e));
}

void
printExceptionIDString(Exception e)
{
  Exception ur = e;
  while (ur->import != NULL)
    ur = ur->import;
  if (ur->corba_rep_id == NULL)
    printf("'ilu:%s.%s'", interface_name(ur->interface), name_base_name(e->name));
  else
    printf("'%s'", ur->corba_rep_id);
}

void
printTypeIoFuncName(Type t, const char *prefix)
{
	Type	urt	= ur_type(t);

	printIfcPrefix(urt->interface);
	printf(fmtFuncIo, prefix, getTypeName(urt));
}

void printTypeRef(Type t)
{
  printIfcPrefix(type_interface(ur_type(t)));
  printf("%s", getTypeName(ur_type(t)));
}

void sprintTypeRef(char *buf, Type t)
{
  sprintIfcPrefix(buf, type_interface(ur_type(t)));
  sprintf(buf + strlen(buf), "%s", getTypeName(ur_type(t)));
}

void
printNameScopes(list scopes)
{
  int i;
  string name;
  int size = list_size(scopes);
  printf ("(");
  for (i = 0;  i < size;  i++) {
    name = (string) list_ref(scopes, i);
    printf("'%s',", name);
  };
  printf(")");
}

static unsigned long indent_level = 0;

void
  modifyIndentLevel (int change)
{
  indent_level += change;
}

static char *current_file = NULL;
static int current_line = 0;

void
  setFileAndLine (char *filename, int line)
{
  current_file = filename;
  current_line = line;
}

int
indentedPrintf(const char *formatSpec,...)
{
  int		result = 0;
  char *	p;
  va_list       ap;
  static boolean	newlined = TRUE;

  if ((p = strchr(formatSpec, '\n')) != NULL) {
    while (*++p != 0) {
      if (*p != '\n') {
	fprintf (stderr, "%s printf statement with non-trailing newline\n", 0);
	return 0;
      };
    }
  }
  va_start(ap, formatSpec);
  if (newlined)
    result += fprintf(stdout, "%*.*s", indent_level * 4, indent_level * 4, "");
  result += vprintf(formatSpec, ap);
  va_end(ap);
  newlined = (p != NULL);
  return result;
}
