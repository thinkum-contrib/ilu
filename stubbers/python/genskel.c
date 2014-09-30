
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
$Id: genskel.c,v 1.31 1999/08/03 01:50:37 janssen Exp $
*/
/* Last edited by Mike Spreitzer September 17, 1998 2:53 pm PDT */

#include <stdio.h>
#include "iluptype.h"
#include "io.h"
#include "manifest.h"
#include "name.h"
#include "util.h"
#include "package.h"
#include "genskel.h"

static char *	sysIfcName = "sys";
static char *	tracebackIfcName = "traceback";
static char *	surrogateModuleName = "_theSurrogateModule";

/************************************************************************/

static void
skImported(Imported i)
{
  printImportIfc(i->name, TRUE);
}

static void
skImportSurrogate (char *name)
{
  printImportIfc (name, FALSE);
  printf("%s = %s;\ndel %s;\n\n", surrogateModuleName, name, name);	 
}

/************************************************************************/

static void
skArgInput(Argument a)
{
	if (a->direction == In || a->direction == InOut)
	{
		printf("    %s = ", getArgumentName(a));
		ioTypeInput(a->type);
		newline();
	}
}

static void
skMethodReceiveRequest(Procedure m)
{
	Type	tClass	= m->object;
	Class	c	= tClass->description->structuredDes.object;

	printf("    %s = ", nameVarSelf);
	if (c->singleton != 0)
		printf("%s.GetSingleton(%s)\n", nameModuleIlu, nameVarCall);
	else
	{
		ioObjDiscrimInput(tClass);
		newline();
	}
	list_enumerate(m->arguments, (EnumProc) skArgInput, NULL);
}

static void
skMethodException(Exception e, int *pCount)
{
	Type	et	= e->import ? e->import->type : e->type;
	char	namebuf[2048];

	printf("    except ");
	printFullExceptionName(e);
	if (et)
		printf(", %s", nameVarExceptValue);
	printf(":\n");

	++*pCount;
	printf("\t%s.BeginException(%s, %d, %s.BeginSizingException(%s, %d) + ",
	       nameModuleIlu, nameVarCall, *pCount, nameModuleIlu, nameVarCall, *pCount);
	if (et)
	{
		printf("\\\n");
		printf("\t  ");
#ifdef ILU_CORBA_PYTHON_MAPPING
		if (isPrefixOf(prefixIdlExcType, type_name(et)))
		  sprintf (namebuf, "%s", nameVarExceptValue);
		else
		  sprintf (namebuf, "%s.value", nameVarExceptValue);
		ioTypeOutSize(et, namebuf, prefixFuncSizeOf);
#else
		ioTypeOutSize(et, nameVarExceptValue, prefixFuncSizeOf);
#endif /* def ILU_CORBA_PYTHON_MAPPING */
	}
	else {
	  printf(" 0");
	}
	printf(")\n");

	if (et)
	{
		printf("\t");
#ifdef ILU_CORBA_PYTHON_MAPPING
		ioTypeOutSize(et, namebuf, prefixFuncOutput);
#else
		ioTypeOutSize(et, nameVarExceptValue, prefixFuncOutput);
#endif /* def ILU_CORBA_PYTHON_MAPPING */
		newline();
	}

	printf("\t%s.FinishException(%s)\n", nameModuleIlu, nameVarCall);
	printf("\treturn\n");
}

static void
skMethodDefaultExceptionHandler(Procedure m)
{
#ifdef ILU_CORBA_PYTHON_MAPPING

  printf ("    except %s['%s'].SystemException, %s:\n", nameVarImports, nameModuleCORBA, nameVarExceptValue);
  printf ("\t%s.CaughtSystemException(%s, %s.__class__.__name__, %s.minor, %s.completed)\n",
	  nameModuleIlu, nameVarCall, nameVarExceptValue, nameVarExceptValue, nameVarExceptValue);
  printf ("\treturn\n");

#endif /* def ILU_CORBA_PYTHON_MAPPING */

  printf("    except:\n");
  printf("\t%s.CaughtUnexpectedException(%s)\n", nameModuleIlu,
	 nameVarCall);
}

typedef struct
{
	int		nResults;
	const char *	prefix;
	int		count;
} RosRock;

static void
skResultOutSize(Type t, RosRock *r)
{
	char		buffer[64];
	const char *	argName;

	if (r->nResults == 1)
		argName = nameVarResult;
	else
	{
		sprintf(buffer, "%s[%d]", nameVarResult, r->count);
		argName = buffer;
	}
	r->count += 1;
	if (r->prefix == prefixFuncOutput) {
	  printf("    ");
	} else if (r->count > 1) {
	  printf("\\\n");
	  printf("      + ");
	}
	ioTypeOutSize(t, argName, r->prefix);
	if (r->prefix == prefixFuncOutput)
		newline();
}

static void
skArgOutSize(Argument a, RosRock *r)
{
	if (a->direction == Out || a->direction == InOut)
		skResultOutSize(a->type, r);
}

static void
skResultsOutSize(Procedure m, const int nResults, const char *prefix)
{
	if (nResults > 0)
	{
		RosRock	rock;

		rock.nResults = nResults;
		rock.prefix = prefix;
		rock.count = 0;

		if (m->returnType)
			skResultOutSize(m->returnType, &rock);
		list_enumerate(m->arguments, (EnumProc) skArgOutSize, &rock);
	}
	else if (prefix == prefixFuncSizeOf)
		printf("0");
}

static void
skMethodSendReply(Procedure m, const int nResults)
{
	printf("    %s.BeginReply(%s, %s.%s, %s.BeginSizingReply(%s, %s.%s) + ",
		nameModuleIlu, nameVarCall, nameModuleIlu,
		booleanImage(list_size(m->exceptions) != 0),
		nameModuleIlu, nameVarCall, nameModuleIlu,
		booleanImage(list_size(m->exceptions) != 0));
	skResultsOutSize(m, nResults, prefixFuncSizeOf);
	printf(")\n");
	skResultsOutSize(m, nResults, prefixFuncOutput);
	printf("    %s.FinishReply(%s)\n", nameModuleIlu, nameVarCall);
}

static void
skMethodFinishRequest(Procedure m)
{
	char *	name		= getProcedureName(m);
	int	nResults	= methodResultCount(m);

	printf("    %s.RequestRead(%s)\n", nameModuleIlu, nameVarCall);
	if (m->asynch != 0)
	  {
	    printf("    ");
	  }
	else
	  {
	    printf("    try:\n");
	    printf("\t");
	    if (nResults > 0)
	      printf("%s = ", nameVarResult);
	  }
	printf("%s.%s(", nameVarSelf, name);
	printArgList(m->arguments, 0);
	printf(")\n");

	if (m->asynch == 0)
	  {
	    int	nExcepts	= 0;
	    
	    list_enumerate(m->exceptions, (EnumProc) skMethodException,
			   &nExcepts);
	    skMethodDefaultExceptionHandler(m);
	    skMethodSendReply(m, nResults);
	  }
	else
	  {
	    printf("    %s.NoReply(%s)\n", nameModuleIlu, nameVarCall);
	  }

}

static void
skMethod(Procedure m, const char *className)
{
	char *	name	= getProcedureName(m);

	newline();
	printf("def ");
	printf(fmtFuncSkel, className, name);
	printf("(%s):\n", nameVarCall);
	skMethodReceiveRequest(m);
	skMethodFinishRequest(m);
}

static void
skClassMethodSkeletons(Type t)
{
	Class	c	= t->description->structuredDes.object;
	char *	name	= getTypeName(t);

	list_enumerate(c->methods, (EnumProc) skMethod, name);
}

/************************************/

static void
skPrintMethodSkeletonName(Procedure m, const char *className)
{
	char *	name	= getProcedureName(m);

	printf("\\\n");
	printf("  ");
	printf(fmtFuncSkel, className, name);
	printf(",");
}

static void
skRegisterClassMethodSkeletons(Type t)
{
	Class	c	= t->description->structuredDes.object;
	char *	name	= getTypeName(t);

	newline();
	printf("%s.RegisterSkeletons(", nameModuleIlu);
	printClassVarName(t, nameVarClass);
	printf(", (");
	list_enumerate(c->methods, (EnumProc) skPrintMethodSkeletonName, name);
	printf("))\n");
}

/************************************/

static void
skClassRecord(Type t)
{
	printf("    %s = ", nameVarClass);
	printClassVarName(t, nameVarClass);
	newline();
}

static void
skDummyMethod(Procedure m)
{
	newline();
	printf("    def %s(%s", getProcedureName(m), nameVarSelf);
	printArgList(m->arguments, 1);
	printf("):\n");
	printf("\traise %s.%s, '%s'\n", nameModuleIlu, nameExceptUnimpl,
		getIslProcedureName(m));
}

static void
skClassClass(Type t)
{
	Class	c	= t->description->structuredDes.object;

	newline();
	printf("class %s(%s.%s):\n", getTypeName(t),
		nameModuleIlu, nameClassSkel);
	skClassRecord(t);
	list_enumerate(c->methods, (EnumProc) skDummyMethod, NULL);
}

/************************************/

static void
skClass(Type t)
{
	skClassMethodSkeletons(t);
	skRegisterClassMethodSkeletons(t);
	skClassClass(t);
}

/************************************/

static void
skAliasObjectType(Type t)
{
	Type	undert	= under_type(t);

	newline();
	printf("%s = ", getTypeName(t));
	if (undert->interface != currentIfc)
	{
	  printf ("%s['", nameVarImports);
	  printf (fmtSkelModuleName, getSimpleInterfaceName(undert->interface));
	  printf ("'].");
	}
	printf("%s", getTypeName(undert));
	newline();
}

static void
skAliasType(Type t)
{
	switch (type_description(t)->type)
	{
	case object_Type:		skAliasObjectType(t);	break;
	default:			/* null */		break;
	}
}

/************************************/

static void
skType(Type t)
{
	if (t->description)
	{
		switch (t->description->type)
		{
		case object_Type:	skClass(t);		break;
		default:		/* null */		break;
		}
	}
	else
		skAliasType(t);
}

/************************************************************************/

#ifdef ILU_CORBA_PYTHON_MAPPING

static void
skSpace (namespace ns)
{
  if (ns->type != idlInterface)
    return;
  skType (ns->d.interface.ifc);
  if (list_size(ns->subspaces) > 0) {
    modifyIndentLevel (+1);
    list_enumerate (ns->subspaces, (EnumProc) skSpace, NULL);
    modifyIndentLevel (-1);
  }
}

void
generateSkel(namespace ns)
{
  currentIfc = namespace_interface(ns);
  generatingSkeleton = TRUE;
  printBanner("Skeletons", namespace_interface(ns));
  printf("import %s;\n", sysIfcName);
  printImportTable();
  printImportIfc(interface_name(namespace_interface(ns)), FALSE);
  list_enumerate(namespace_imports(ns), (EnumProc) skImported, NULL);
  list_enumerate(ns->types,   (EnumProc) skType, NULL);
  list_enumerate(ns->subspaces, (EnumProc) skSpace, NULL);
  generatingSkeleton = FALSE;
}

#else

void
generateSkel(Interface ifc, list localtypes)
{
  char *ifcName;

  currentIfc = ifc;
  ifcName = interface_name(ifc);
  generatingSkeleton = TRUE;
  printBanner("Skeletons", ifc);
  printf("import %s;\n", sysIfcName);
  printImportTable();
  printImportIfc(ifcName, FALSE);
  list_enumerate(ifc->imports, (EnumProc) skImported, NULL);
  list_enumerate(localtypes,   (EnumProc) skType, NULL);
  generatingSkeleton = FALSE;
}

#endif /* def ILU_CORBA_PYTHON_MAPPING */
