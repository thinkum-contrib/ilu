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
$Id: package.c,v 1.7 1999/09/02 06:09:35 janssen Exp $
*/

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "iluptype.h"
#include "manifest.h"
#include "name.h"
#include "util.h"
#include "package.h"

#include <version.h>

static namespace
  new_namespace (char *name, idltype type, namespace parent)
{
  namespace ns;
  ns = (namespace) malloc(sizeof(*ns));
  ns->type = type;
  ns->name = ilu_strdup(name);
  ns->parent = parent;
  ns->ifc = NULL;
  ns->subspaces = new_list();
  ns->types = new_list();
  ns->exceptions = new_list();
  ns->constants = new_list();
  if (type == idlModule) {
    ns->d.module.brand = NULL;
    ns->d.module.id = NULL;
  } else {
    ns->d.interface.ifc = NULL;
  }
  return ns;
}

Interface
namespace_interface (namespace ns)
{
  if (ns == NULL)
    return NULL;
  else if (ns->ifc != NULL)
    return ns->ifc;
  else
    return namespace_interface(ns->parent);
}

list
namespace_imports (namespace ns)
{
  Interface ifc = namespace_interface(ns);

  if (ifc)
    return ifc->imports;
  else
    return NULL;
}

char *
getSimpleNamespaceName (namespace ns)
{
  return ns->name;
}

static boolean
  matchNamespaceName (namespace ns, string name)
{
  return (strcmp(ns->name, name) == 0);
}

static namespace
  locate_namespace (namespace top, list scoping)
{
  int i, l;
  namespace startspace, ns2;
  char *name;

  for (l = list_size(scoping) - 1, startspace = top, i = 0;  i < l;  i++) {
    name = list_ref(scoping, i);
    ns2 = (namespace) list_find (startspace->subspaces, (iluparser_FindProc) matchNamespaceName, name);
    if (ns2 == NULL) {
      ns2 = new_namespace(name, idlModule, startspace);
      list_insert(startspace->subspaces, ns2);
    }
    startspace = ns2;
  }
  return startspace;
}

static void
  addTypeToTree (refany type_ptr, refany namespace_tree_ptr)
{
  Type type = (Type) type_ptr;
  namespace ns1, ns2;

  if (type->builtIn)
    return;
  if (strcmp("ilu", interface_name(type_interface(type))) == 0)
    return;
  if (type->importInterfaceName != NULL)
    return;

  /* locate appropriate namespace */
  ns1 = locate_namespace((namespace) namespace_tree_ptr, type->scoping);
  if (type_kind(type) == object_Type) {
    char *name = list_ref(type->scoping, list_size(type->scoping) - 1);
    ns2 = (namespace) list_find (ns1->subspaces, (iluparser_FindProc) matchNamespaceName, name);
    if (ns2 == NULL) {
      ns2 = new_namespace(name, idlInterface, ns1);
      list_insert(ns1->subspaces, ns2);
    } else if (ns2->type == idlModule) {
      ns2->type = idlInterface;
    }
    ns2->d.interface.ifc = type;
  } else {
    list_insert (ns1->types, type);
  }
}

static
  Interface exception_interface (Exception e)
{
  Exception ep = e;

  while (ep->import != NULL)
    ep = ep->import;
  return (ep->interface);
}

static void
  addExceptionToTree (refany e_ptr, refany namespace_tree_ptr)
{
  Exception excn = (Exception) e_ptr;
  namespace ns;

  if (strcmp("ilu", interface_name(exception_interface(excn))) == 0)
    return;
  if (excn->importInterfaceName != NULL)
    return;

  ns = locate_namespace((namespace) namespace_tree_ptr, excn->scoping);
  list_insert (ns->exceptions, excn);
}

static
  Interface constant_interface (Constant c)
{
  Constant ep = c;

  while (ep->import != NULL)
    ep = ep->import;
  return (ep->interface);
}

static void
  addConstantToTree (refany e_ptr, refany namespace_tree_ptr)
{
  Constant c = (Constant) e_ptr;
  namespace ns, ns2;

  if (strcmp("ilu", interface_name(constant_interface(c))) == 0)
    return;
  if (c->importInterfaceName != NULL)
    return;

  ns = locate_namespace((namespace) namespace_tree_ptr, c->scoping);
  list_insert (ns->constants, c);
}

static void
  check_submodules (namespace ns)
{
  cardinal i, l;
  boolean result = FALSE;
  namespace ns2;

  ns->d.module.has_submodules = FALSE;
  for (l = list_size(ns->subspaces), i = 0;  i < l;  i++) {
    ns2 = (namespace) list_ref(ns->subspaces, i);
    if (ns2->type == idlModule) {
      ns->d.module.has_submodules = TRUE;
      check_submodules (ns2);
    };
  };
}

static boolean
  isAncestorOf (Type t1, Type t2)
/* return TRUE if t2 is an ancestor of t1.  Both t1 and t2 are object types */
{
  if (t1 == t2) return TRUE;
  return (list_find(class_object(t1)->superclasses, (iluparser_FindProc) isAncestorOf, t2) != NULL);
}

static void
  sort_subspaces (namespace ns)
/* sort so that interface types which are supertypes of other interface
   types appear in the subspaces list of the namespace before those
   descendant interface types */
{
  int i, j, lim;
  namespace tmp;

  list_enumerate (ns->subspaces, (EnumProc) sort_subspaces, NULL);
  for (i = 0, lim = list_size(ns->subspaces);  i < lim;  i++) {
    for (j = i + 1;  j < lim;  j++) {
      if (isAncestorOf(((namespace)list_ref(ns->subspaces, i))->d.interface.ifc,
		       ((namespace)list_ref(ns->subspaces, j))->d.interface.ifc)) {
	tmp = list_ref(ns->subspaces, i);
	list_ref_set(ns->subspaces, i, list_ref(ns->subspaces, j));
	list_ref_set(ns->subspaces, j, tmp);
	j = i;
      }
    }
  }
  /*
  {
    int i;
    fprintf (stderr, "%s:\n", ns->name);
    for (i = 0;  i < list_size(ns->subspaces);  i++) {
      fprintf (stderr, "%d: %s\n", i, type_name(((namespace) list_ref(ns->subspaces, i))->d.interface.ifc));
    }
    fprintf (stderr, "\n");
  }
  */
}

namespace
  idlsort (Interface ifc)
{
  namespace	namespace_tree;

  namespace_tree = new_namespace("__top__", idlModule, NULL);

  namespace_tree->ifc = ifc;

  list_enumerate(ifc->types, addTypeToTree, namespace_tree);
  list_enumerate(ifc->constants, addConstantToTree, namespace_tree);
  list_enumerate(ifc->exceptions, addExceptionToTree, namespace_tree);

  sort_subspaces(namespace_tree);
  check_submodules (namespace_tree);

  return namespace_tree;
}

static void
  printScoping (list scoping)
{
  cardinal i, lim;
  for (lim = list_size(scoping), i = 0;  i < lim;  i++) {
    printf ("%s\"%s\"", (i > 0) ? ", " : "", (string) list_ref(scoping, i));
  }
}

static void
  printType (Type t, refany junk)
{
  printf ("%s  (%s)  (", type_name(t), type_uid(t));
  printScoping (t->scoping);
  printf (")\n");
}

static void
  printException (Exception e, refany junk)
{
  printf ("%s : %s  (", exception_name(e), exception_type(e) ? type_name(exception_type(e)) : "(none)");
  printScoping (e->scoping);
  printf (")\n");
}

static void
  printConstant (Constant e, refany junk)
{
  printf ("%s : %s  (", constant_name(e), type_name(constant_type(e)));
  printScoping (e->scoping);
  printf (")\n");
}

void
printSort (namespace n)
{
  printf ("%s \"%s\"%s:\n", (n->type == idlModule) ? "Module" : "Interface", n->name,
	  ((n->type == idlModule) && (n->d.module.has_submodules)) ? " *" : "");
  modifyIndentLevel (+1);
  printf ("(Types):\n");
  modifyIndentLevel (+1);
  list_enumerate (n->types, (iluparser_EnumProc) printType, 0);
  modifyIndentLevel (-1);
  printf ("(Constants):\n");
  modifyIndentLevel (+1);
  list_enumerate (n->constants, (iluparser_EnumProc) printConstant, 0);
  modifyIndentLevel (-1);
  printf ("(Exceptions):\n");
  modifyIndentLevel (+1);
  list_enumerate (n->exceptions, (iluparser_EnumProc) printException, 0);
  modifyIndentLevel (-1);
  printf ("(Nested scopes):\n");
  modifyIndentLevel (+1);
  list_enumerate (n->subspaces, (iluparser_EnumProc) printSort, NULL);
  modifyIndentLevel (-2);
}
