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
$Id: isl2python.c,v 1.24 1999/08/31 16:44:54 janssen Exp $
*/
/* Last edited by Mike Spreitzer September 17, 1998 10:44 pm PDT */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "iluptype.h"
#include "package.h"
#include "manifest.h"
#include "name.h"
#include "util.h"
#include "genstub.h"
#include "genskel.h"
#include "prttree.h"

#ifdef _IS_POSIX
#include <unistd.h>
#include <errno.h>
#endif /* _IS_POSIX */

typedef struct
{
	list	sortedTypes;
} PtRock;

static void
processType(Type t, PtRock *r)
{
	static list	pendingTypes;

	if (t->importInterfaceName != 0)
		return;
	if (list_find(r->sortedTypes, matchPointer, t) != 0)
		return;
	if (pendingTypes == 0)
		pendingTypes = new_list();
	if (list_find(pendingTypes, matchPointer, t) != 0)
		fatal("internal error: circularity in types");
	list_insert(pendingTypes, t);

	if (t->description)
	{
		switch (t->description->type)
		{
		case object_Type:
		    {
			Class	c	= t->description->structuredDes.object;

			list_enumerate(c->superclasses, (EnumProc) processType,
				r);
		    }
		    break;

		default:
		    /* null */
		    break;
		}
	}
	else
	{
		/* alias */
		processType(t->supertype, r);
	}

	list_remove(pendingTypes, t);
	list_insert(r->sortedTypes, t);
}

static list
normalizeInterface(Interface ifc)
{
	PtRock	rock;

	rock.sortedTypes = new_list();
	list_enumerate(ifc->types, (EnumProc) processType, &rock);
	return rock.sortedTypes;
}

/************************************************************************/

static int		flagPrintTree;
static int		flagGenStub;
static int		flagGenSkel;
static int		flagRmFirst;
static int		flagQuiet = 0;

static void
possibleUnlink (const char *filename)
{
#ifdef _IS_POSIX
  if (flagRmFirst)
    {
      if (unlink(filename) < 0)
	{
	  int e = errno;
	  if (e != ENOENT)
	    fprintf (stderr, "Warning:  couldn't delete existing file \"%s\", %s.\n",
		     filename, strerror(e));
	}
    }
#endif /* _IS_POSIX */
}

#ifdef ILU_CORBA_PYTHON_MAPPING

static void
ensurePath (const char *dir)
{
  struct stat buf;
  if ((stat(dir, &buf) != 0) && (errno == ENOENT)) {
    mkdir (dir, 0777);
    if (stat(dir, &buf) != 0)
      fatal("can't make directory %d: errno %d (%s)\n",
	    dir, errno, strerror(errno));
  } else if ((buf.st_mode & S_IFDIR) == 0) {
    fatal ("specified directory %s exists but is not a directory!\n", dir);
  }
}

static void
reopenStdout(const char *what, const char *fmtModuleName, namespace ns, char *pathDir)
{
	char	moduleName[2048];
	char	fileName[2048];

	if (pathDir)
	  ensurePath(pathDir);
	if (ns->d.module.has_submodules) {
	  strcpy (fileName, pathDir);
	  strcat (fileName, "/");
	  strcat (fileName, "__init__.py");
	} else {
	  /* leaf module; just create a file with that name */
	  if (pathDir) {
	    strcpy (fileName, pathDir);
	    strcat (fileName, "/");
	  } else
	    fileName[0] = 0;
	  sprintf (fileName + strlen(fileName), fmtModuleName, getSimpleNamespaceName(ns));
	  strcat (fileName, ".py");
	}
	if (! flagQuiet)
	  fprintf(stderr, "%s for %s \"%s\" to %s ...\n", what,
		  (ns->type == idlModule) ? "module" : "interface",
		  ns->name, fileName);
	possibleUnlink(fileName);
	if (freopen(fileName, "w", stdout) == NULL)
		sysFatal(fileName);
}

static void
  generate2 (namespace ns, char *pathDir)
{
  int i, lim;

  /* if namespace is terminal, generate into file named for the namespace.
     Otherwise, generate into "__init__.py". */

  if (ns->type == idlInterface)
    return;

  else if ((strcmp(ns->name, "__top__") == 0) &&
	   (list_size(ns->types) == 0) &&
	   (list_size(ns->constants) == 0) &&
	   (list_size(ns->exceptions) == 0)) {
    for (i = 0, lim = list_size(ns->subspaces);  i < lim;  i++) {
      generate2 ((namespace) list_ref(ns->subspaces, i), pathDir);
    }
  }

  else if (ns->type == idlModule && ns->d.module.has_submodules) {
    char newPathDir[2048];
    if (pathDir)
      strcpy(newPathDir, pathDir);
    else
      newPathDir[0] = 0;
    strcat (newPathDir, getSimpleNamespaceName(ns));
    if ((list_size(ns->types) != 0) ||
	(list_size(ns->constants) != 0) ||
	(list_size(ns->exceptions) != 0)) {
      if (flagGenStub) {
	reopenStdout ("basic stubs", fmtStubModuleName, ns, newPathDir);
	generateStub (ns);
      }
      if (flagGenSkel) {
	reopenStdout ("server-side stubs", fmtSkelModuleName, ns, newPathDir);
	generateSkel (ns);
      }
    }
    for (i = 0, lim = list_size(ns->subspaces);  i < lim;  i++) {
      generate2 ((namespace) list_ref(ns->subspaces, i), newPathDir);
    }
  } 

  else {	/* ns->type == idlModule && !ns->d.module.has_submodules */
    if (flagGenStub) {
      reopenStdout ("basic stubs", fmtStubModuleName, ns, pathDir);
      generateStub (ns);
    }
    if (flagGenSkel) {
      reopenStdout ("server-side stubs", fmtSkelModuleName, ns, pathDir);
      generateSkel (ns);
    }
  }

}

static void
generate(Interface ifc, char *pathDir)
{
  namespace ns = idlsort(ifc);

  if (flagPrintTree)
    printSort (ns);

  generate2 (ns, pathDir);
}

#else

static void
reopenStdout(const char *what, const char *fmtModuleName, Interface ifc, char *pathDir)
{
	char	moduleName[2048];
	char	fileName[2048];

	sprintf(moduleName, fmtModuleName, getSimpleInterfaceName(ifc));
	if (pathDir) {
	  strcpy (fileName, pathDir);
	  strcat (fileName, "/");
	} else
	  fileName[0] = 0;
	sprintf(fileName + strlen(fileName), fmtFileName, moduleName);
	if (! flagQuiet)
	  fprintf(stderr, "%s for interface \"%s\" to %s ...\n", what,
		  interface_name(ifc), fileName);
	possibleUnlink(fileName);
	if (freopen(fileName, "w", stdout) == NULL)
		sysFatal(fileName);
}

static void
generate(Interface ifc, char *pathDir)
{
	list localtypes = normalizeInterface(ifc);

	if (flagPrintTree)
		printTree(ifc, localtypes);

	if (flagGenStub)
	{
		if (flagGenStub == 'f')
			reopenStdout("client stubs", "%s", ifc, pathDir);
		generateStub(ifc, localtypes);
	}

	if (flagGenSkel)
	{
		if (flagGenSkel == 'f')
			reopenStdout("server stubs", fmtSkelModuleName, ifc, pathDir);
		generateSkel(ifc, localtypes);
	}
}

#endif /* def ILU_CORBA_PYTHON_MAPPING */

/************************************/

typedef enum
{
	OptTree, OptStub, OptSkel, OptRm, OptDir, OptQuiet, OptInclude,
	OptNull
} Option;

typedef struct
{
	char *	name;
	Option	opt;
} OptTuple;

static OptTuple		optionTable[] =
{
    {	"tree",		OptTree	},
    {	"stub",		OptStub	},
    {	"skel",		OptSkel	},
    {   "removefirst",	OptRm   },
    {   "dir",		OptDir  },
    {   "quiet",	OptQuiet},
    {   "I",		OptInclude },
};

static Option
getOption(char *name)
{
	int	i;

	for (i = 0; i < sizeof optionTable / sizeof optionTable[0]; i++)
	{
		if (strcmp(name, optionTable[i].name) == 0)
			return optionTable[i].opt;
	}
	return OptNull;
}

/************************************/

static void
usage(void)
{
	fprintf(stderr, "usage: %s [-I DIR ... ] [-removefirst] [-tree] [-dir DIRECTORY] [-quiet] files\n", programName);
	exit(0);
}

int
main(int argc, char **argv)
{
	int	optind	= 1;
	char *	pathDir = NULL;
	list includeDirs;

	includeDirs = new_list();

	if ((programName = iluparser_GetProgramName(argv[0])) == NULL)
	  programName = argv[0];

	while (optind < argc && argv[optind][0] == '-')
	{
		char *	opt	= argv[optind++];

		switch (getOption(opt + 1))
		{
		case OptTree:		flagPrintTree = 1;				break;
		case OptStub:		flagGenStub = 's';				break;
		case OptSkel:		flagGenSkel = 's';				break;
		case OptRm:		flagRmFirst = 1;				break;
		case OptDir:		pathDir = argv[optind++];			break;
		case OptInclude:	list_insert(includeDirs, argv[optind++]);	break;
		case OptQuiet:		flagQuiet = 1;					break;
		default:		usage();
		}
	}
	if (flagGenStub == 0 && flagGenSkel == 0)
		flagGenStub = flagGenSkel = 'f';

	iluparser_RegisterInterfaceDirectories(includeDirs);

	while (optind < argc)
	{
		char *	fileName= argv[optind++];
		list	s;

		if ((s = ParseFile(fileName)) == 0)
			fatal("Couldn't find or parse %s.\n", fileName);
		list_enumerate(s, (EnumProc) generate, pathDir);

#if 0

		{
		  namespace ns;
		  ns = idlsort (list_ref(s, 0));
		  if (ns) {
		    printSort (ns);
		  } else {
		    fatal ("Couldn't sort interface %s\n", interface_name((Interface) list_ref(s, 0)));
		  }
		}

#endif
	}

	return 0;
}
